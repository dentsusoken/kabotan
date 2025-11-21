(in-package :kabotan)

;;; ============================================================================
;;; Character Chat API Handler
;;; ============================================================================
;;;
;;; This handler implements both streaming and non-streaming character chat
;;; endpoints. Character chat allows users to have conversations with Halloween
;;; characters (Dracula, Witch, Jack-o'-lantern) in multiple languages.
;;;
;;; Streaming vs Non-Streaming:
;;; ----------------------------
;;; - Streaming: Real-time response delivery via SSE
;;; - Non-streaming: Complete response after generation
;;;
;;; The streaming version provides better UX for long responses.
;;;
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Parameter Extraction
;;; ----------------------------------------------------------------------------

(defun extract-character-chat-params (params)
  "Extract and validate parameters for character chat requests.
   
   Parameters:
   -----------
   - params: Alist of request parameters
   
   Returns:
   --------
   - (values param-plist nil) on success
   - (values nil error-message) on validation failure
   
   Param plist contains:
   - :language - Validated language code
   - :character - Normalized character name
   - :messages - Parsed message list"
  (let ((language (extract-and-validate-language params))
        (character (get-param params "character" "dracula"))
        (messages-json (get-param params "messages")))
    
    ;; Normalize character (jack-o-lantern variations)
    (when (or (string= character "jack")
              (string= character "jack-o-lantern"))
      (setf character "jack"))
    (unless (member character '("dracula" "witch" "jack") :test #'string=)
      (setf character "dracula"))
    
    ;; Validate messages parameter
    (if (not messages-json)
        (values nil "Messages parameter is required")
        
        ;; Parse and validate messages JSON
        (handler-case
            (let* ((messages-alist (cl-json:decode-json-from-string messages-json))
                   (messages-list (loop for msg-alist in messages-alist
                                       for role = (cdr (assoc :role msg-alist))
                                       for content = (cdr (assoc :content msg-alist))
                                       when (and role content)
                                       collect (list :role role 
                                                   :content (sanitize-input content)))))
              
              ;; Validate that we have at least one message
              (if (null messages-list)
                  (values nil "Messages array is empty")
                  (values (list :language language
                               :character character
                               :messages messages-list)
                         nil)))
          (error (condition)
            (log-error "character-chat JSON parse error" condition)
            (values nil "Invalid messages JSON format"))))))

;;; ----------------------------------------------------------------------------
;;; Business Logic
;;; ----------------------------------------------------------------------------

(defun process-character-chat (params)
  "Process character chat request with validated parameters.
   
   Parameters:
   -----------
   - params: Plist with :language, :character, :messages
   
   Returns:
   --------
   - LLM response string on success
   - nil on failure"
  (let* ((language (getf params :language))
         (character (getf params :character))
         (messages (getf params :messages))
         (system-prompt (build-character-chat-system-prompt character language)))
    
    (call-llm-with-messages-retry messages
                                 :temperature 0.85
                                 :system-prompt system-prompt)))

;;; ----------------------------------------------------------------------------
;;; Public Handlers
;;; ----------------------------------------------------------------------------

(defun handle-character-chat-streaming (params)
  "Handles streaming character chat API requests with HTMX SSE extension.
   
   Endpoint: GET /api/character-chat-stream
   
   This handler:
   1. Extracts session ID from cookies
   2. Retrieves conversation history from session
   3. Adds user message to history
   4. Streams LLM response as HTML fragments
   5. Adds assistant response to history after completion
   6. Returns SSE stream with HTML fragments
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - Clack streaming response with SSE format (200)
   - Error response with HTML body (400 or 500)
   
   SSE Event Format (HTMX):
   ------------------------
   Content: event: message\\ndata: <span>text</span>\\n\\n
   Done: event: done\\ndata: \\n\\n
   Error: event: error\\ndata: <div class=\"alert alert-error\">...</div>\\n\\n"
  (handler-case
      (let* ((session-id (extract-session-id-from-params params))
             (session (get-or-create-session session-id))
             (language (detect-language-from-request params))
             (character (get-param params "character" "dracula"))
             (message (get-param params "message")))
        
        ;; Normalize character name (keep jack-o-lantern as jack internally)
        (when (string= character "jack-o-lantern")
          (setf character "jack"))
        (unless (member character '("dracula" "witch" "jack") :test #'string=)
          (setf character "dracula"))
        
        ;; Validate message parameter
        (if (not (and message (validate-non-empty-string message)))
            (build-validation-error-response "Message parameter is required"
                                            language
                                            :streaming-p t)
            
            (let* ((sanitized-message (sanitize-input message))
                   ;; Add user message to session history
                   (_ (add-to-conversation-history 
                       (session-id session)
                       "character-chat"
                       "user"
                       sanitized-message))
                   ;; Get conversation history for LLM
                   (history (get-conversation-history 
                            (session-id session)
                            "character-chat"))
                   ;; Build messages list for LLM
                   (messages (mapcar (lambda (msg)
                                      (list :role (conversation-message-role msg)
                                            :content (conversation-message-content msg)))
                                    history))
                   ;; Process chat with LLM
                   (system-prompt (build-character-chat-system-prompt character language)))
              
              (declare (ignore _))
              
              (create-sse-response
               (lambda (writer)
                 ;; First, send the user message HTML
                 (funcall writer 
                         (format-sse-event "message"
                          (generate-chat-message "user" sanitized-message character language)))
                 
                 ;; Send a special event to signal start of assistant response
                 (funcall writer 
                         (format-sse-event "start-response"
                          (cl-json:encode-json-to-string 
                           (list (cons :character (get-character-display-name character language))))))
                 
                 ;; Accumulate response for session history
                 (let ((accumulated-response (make-array 0 
                                                        :element-type 'character 
                                                        :fill-pointer 0 
                                                        :adjustable t)))
                   (let ((result (call-llm-with-messages-streaming 
                                  messages
                                  (lambda (chunk)
                                    ;; Accumulate chunk for history
                                    (loop for char across chunk
                                          do (vector-push-extend char accumulated-response))
                                    ;; Send each chunk as plain text wrapped in span
                                    (funcall writer 
                                            (format-sse-event "message"
                                             (format nil "<span>~A</span>" (escape-html chunk)))))
                                  :temperature 0.85
                                  :system-prompt system-prompt)))
                     
                     (if (eq result :error)
                         ;; Send HTML error alert as 'error' event
                         (funcall writer (format-sse-html-error "LLM service error" language))
                         (progn
                           ;; Add assistant response to session history
                           (when (> (length accumulated-response) 0)
                             (add-to-conversation-history 
                              (session-id session)
                              "character-chat"
                              "assistant"
                              accumulated-response))
                           ;; Send empty 'done' event
                           (funcall writer (format-sse-event "done" "")))))))))))
    (error (condition)
      (log-error "character-chat-streaming endpoint" condition)
      (build-service-error-response (format nil "~A" condition)
                                   (detect-language-from-request params)
                                   :streaming-p t))))

(defun handle-character-chat-request (params)
  "Handles character chat API requests with session-based conversation history.
   
   Endpoint: POST /api/character-chat
   
   This handler:
   1. Extracts session ID from cookies
   2. Retrieves conversation history from session
   3. Adds user message to history
   4. Processes chat with LLM
   5. Adds assistant response to history
   6. Returns HTML with both user and assistant messages
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)"
  (handler-case
      (let* ((session-id (extract-session-id-from-params params))
             (session (get-or-create-session session-id))
             (language (detect-language-from-request params))
             (character (get-param params "character" "dracula"))
             (message (get-param params "message")))
        
        ;; Normalize character name (keep jack-o-lantern as jack internally)
        (when (string= character "jack-o-lantern")
          (setf character "jack"))
        (unless (member character '("dracula" "witch" "jack") :test #'string=)
          (setf character "dracula"))
        
        ;; Validate message parameter
        (if (not (and message (validate-non-empty-string message)))
            (build-validation-error-response "Message parameter is required"
                                            language
                                            :streaming-p nil)
            
            (let* ((sanitized-message (sanitize-input message))
                   ;; Add user message to session history
                   (_ (add-to-conversation-history 
                       (session-id session)
                       "character-chat"
                       "user"
                       sanitized-message))
                   ;; Get conversation history for LLM
                   (history (get-conversation-history 
                            (session-id session)
                            "character-chat"))
                   ;; Build messages list for LLM
                   (messages (mapcar (lambda (msg)
                                      (list :role (conversation-message-role msg)
                                            :content (conversation-message-content msg)))
                                    history))
                   ;; Process chat with LLM
                   (system-prompt (build-character-chat-system-prompt character language))
                   (result (call-llm-with-messages-retry messages
                                                        :temperature 0.85
                                                        :system-prompt system-prompt)))
              
              (declare (ignore _))
              
              (if result
                  (progn
                    ;; Add assistant response to session history
                    (add-to-conversation-history 
                     (session-id session)
                     "character-chat"
                     "assistant"
                     result)
                    
                    ;; Return HTML with both user and assistant messages
                    `(200 (:content-type "text/html; charset=utf-8")
                      (,(format-character-chat-response-with-history
                         sanitized-message
                         result
                         character
                         language))))
                  (build-service-error-response "LLM service unavailable"
                                               language
                                               :streaming-p nil)))))
    (error (condition)
      (log-error "character-chat endpoint" condition)
      (build-service-error-response (format nil "~A" condition)
                                   (detect-language-from-request params)
                                   :streaming-p nil))))
