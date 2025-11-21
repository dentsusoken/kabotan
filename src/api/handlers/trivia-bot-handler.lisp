(in-package :kabotan)

;;; ============================================================================
;;; Trivia Bot API Handler
;;; ============================================================================
;;;
;;; This handler implements both streaming and non-streaming trivia bot
;;; endpoints. The trivia bot provides interactive trivia questions and
;;; answers in multiple languages.
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

(defun extract-trivia-bot-params (params)
  "Extract and validate parameters for trivia bot requests.
   
   Parameters:
   -----------
   - params: Alist of request parameters
   
   Returns:
   --------
   - (values param-plist nil) on success
   - (values nil error-message) on validation failure
   
   Param plist contains:
   - :language - Validated language code
   - :messages - Parsed message list"
  (let ((language (extract-and-validate-language params))
        (messages-json (get-param params "messages")))
    
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
                               :messages messages-list)
                         nil)))
          (error (condition)
            (log-error "trivia-bot JSON parse error" condition)
            (values nil "Invalid messages JSON format"))))))

;;; ----------------------------------------------------------------------------
;;; Business Logic
;;; ----------------------------------------------------------------------------

(defun process-trivia-bot (params)
  "Process trivia bot request with validated parameters.
   
   Parameters:
   -----------
   - params: Plist with :language, :messages
   
   Returns:
   --------
   - LLM response string on success
   - nil on failure"
  (let* ((language (getf params :language))
         (messages (getf params :messages))
         (system-prompt (build-trivia-bot-system-prompt language)))
    
    (call-llm-with-messages-retry messages
                                 :temperature 0.75
                                 :system-prompt system-prompt)))

;;; ----------------------------------------------------------------------------
;;; Public Handlers
;;; ----------------------------------------------------------------------------

(defun handle-trivia-bot-streaming (params)
  "Handles streaming trivia bot API requests with HTMX SSE extension.
   
   Endpoint: GET /api/trivia-bot-stream
   
   This handler:
   1. Extracts session ID from cookies
   2. Retrieves conversation history from session
   3. Adds user question to history
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
   Content: event: message\\ndata: <div>...</div>\\n\\n
   Done: event: done\\ndata: \\n\\n
   Error: event: error\\ndata: <div class=\"alert alert-error\">...</div>\\n\\n"
  (handler-case
      (let* ((session-id (extract-session-id-from-params params))
             (session (get-or-create-session session-id))
             (language (detect-language-from-request params))
             (question (get-param params "question")))
        
        ;; Validate question parameter
        (if (not (and question (validate-non-empty-string question)))
            (build-validation-error-response "Question parameter is required"
                                            language
                                            :streaming-p t)
            
            (let* ((sanitized-question (sanitize-input question))
                   ;; Add user question to session history
                   (_ (add-to-conversation-history 
                       (session-id session)
                       "trivia-bot"
                       "user"
                       sanitized-question))
                   ;; Get conversation history for LLM
                   (history (get-conversation-history 
                            (session-id session)
                            "trivia-bot"))
                   ;; Build messages list for LLM
                   (messages (mapcar (lambda (msg)
                                      (list :role (conversation-message-role msg)
                                            :content (conversation-message-content msg)))
                                    history))
                   (system-prompt (build-trivia-bot-system-prompt language)))
              
              (declare (ignore _))
              
              (create-sse-response
               (lambda (writer)
                 ;; First, send the user question HTML
                 (funcall writer 
                         (format-sse-event "message"
                          (generate-trivia-message "user" sanitized-question language)))
                 
                 ;; Send a special event to signal start of assistant response
                 (funcall writer 
                         (format-sse-event "start-response"
                          (cl-json:encode-json-to-string 
                           (list (cons :character "Trivia Bot")))))
                 
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
                                  :temperature 0.75
                                  :system-prompt system-prompt)))
                     
                     (if (eq result :error)
                         ;; Send HTML error alert as 'error' event
                         (funcall writer (format-sse-html-error "LLM service error" language))
                         (progn
                           ;; Add assistant response to session history
                           (when (> (length accumulated-response) 0)
                             (add-to-conversation-history 
                              (session-id session)
                              "trivia-bot"
                              "assistant"
                              accumulated-response))
                           ;; Send empty 'done' event
                           (funcall writer (format-sse-event "done" "")))))))))))
    (error (condition)
      (log-error "trivia-bot-streaming endpoint" condition)
      (build-service-error-response (format nil "~A" condition)
                                   (detect-language-from-request params)
                                   :streaming-p t))))

(defun handle-trivia-bot-request (params)
  "Handles trivia bot API requests with session-based conversation history.
   
   Endpoint: POST /api/trivia-bot
   
   This handler:
   1. Extracts session ID from cookies
   2. Retrieves conversation history from session
   3. Adds user question to history
   4. Processes trivia with LLM
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
             (question (get-param params "question")))
        
        ;; Validate question parameter
        (if (not (and question (validate-non-empty-string question)))
            (build-validation-error-response "Question parameter is required"
                                            language
                                            :streaming-p nil)
            
            (let* ((sanitized-question (sanitize-input question))
                   ;; Add user question to session history
                   (_ (add-to-conversation-history 
                       (session-id session)
                       "trivia-bot"
                       "user"
                       sanitized-question))
                   ;; Get conversation history for LLM
                   (history (get-conversation-history 
                            (session-id session)
                            "trivia-bot"))
                   ;; Build messages list for LLM
                   (messages (mapcar (lambda (msg)
                                      (list :role (conversation-message-role msg)
                                            :content (conversation-message-content msg)))
                                    history))
                   ;; Process trivia with LLM
                   (system-prompt (build-trivia-bot-system-prompt language))
                   (result (call-llm-with-messages-retry messages
                                                        :temperature 0.75
                                                        :system-prompt system-prompt)))
              
              (declare (ignore _))
              
              (if result
                  (progn
                    ;; Add assistant response to session history
                    (add-to-conversation-history 
                     (session-id session)
                     "trivia-bot"
                     "assistant"
                     result)
                    
                    ;; Return HTML with both user and assistant messages
                    `(200 (:content-type "text/html; charset=utf-8")
                      (,(format-trivia-response-with-history
                         sanitized-question
                         result
                         language))))
                  (build-service-error-response "LLM service unavailable"
                                               language
                                               :streaming-p nil)))))
    (error (condition)
      (log-error "trivia-bot endpoint" condition)
      (build-service-error-response (format nil "~A" condition)
                                   (detect-language-from-request params)
                                   :streaming-p nil))))
