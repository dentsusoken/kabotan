(in-package :kabotan)

;;; ============================================================================
;;; HTMX Streaming Handlers
;;; ============================================================================
;;;
;;; This module provides high-level streaming handler functions for HTMX SSE
;;; extension integration. These handlers build on the SSE protocol utilities
;;; to provide convenient abstractions for common streaming patterns.
;;;
;;; Callback Pattern:
;;; -----------------
;;; Streaming functions accept a callback that is invoked for each content chunk:
;;;
;;;   (call-llm-streaming "prompt"
;;;     (lambda (chunk)
;;;       (format t "~A" chunk)))
;;;
;;; The callback receives only the content string, not the full JSON structure.
;;; This simplifies client code and provides a clean abstraction.
;;;
;;; Error Handling:
;;; ---------------
;;; Streaming errors are handled at multiple levels:
;;; 1. Parse errors: Invalid JSON or SSE format returns nil
;;; 2. Connection errors: Logged and propagated to caller
;;; 3. Client disconnects: Detected via "Broken pipe" and handled gracefully
;;;
;;; ============================================================================

(defun create-streaming-handler (prompt-fn callback-fn &key (temperature 0.7))
  "Create a standardized streaming handler for LLM responses.
   
   This function provides a high-level abstraction for creating streaming
   handlers that follow consistent patterns. It handles prompt building,
   LLM streaming, chunk processing, and completion/error events.
   
   The handler integrates with the existing SSE infrastructure and provides
   a clean interface for handler implementations.
   
   Parameters:
   -----------
   - prompt-fn: Function that returns the prompt string
                Called once at the start of streaming
                Example: (lambda () (build-spell-generator-prompt language))
   
   - callback-fn: Function that processes each content chunk
                  Receives (chunk writer) and should send SSE events
                  Example: (lambda (chunk writer) 
                            (funcall writer (format-sse-data chunk)))
   
   - temperature: LLM temperature parameter (default: 0.7)
                  Controls randomness in LLM responses
   
   Returns:
   --------
   - SSE response function compatible with create-sse-response
   
   Behavior:
   ---------
   1. Calls prompt-fn to get the prompt
   2. Initiates streaming LLM call
   3. For each chunk, calls callback-fn with (chunk writer)
   4. On completion, sends done event
   5. On error, sends error event
   
   Example Usage:
   --------------
   (create-streaming-handler
     (lambda () (build-spell-generator-prompt \"en\"))
     (lambda (chunk writer)
       (funcall writer (format-sse-data 
                        (list (cons :chunk chunk)
                              (cons :type \"content\")))))
     :temperature 0.9)"
  (create-sse-response
   (lambda (writer)
     ;; Get the prompt
     (let ((prompt (funcall prompt-fn)))
       ;; Call LLM streaming with the callback
       (let ((result (call-llm-streaming 
                      prompt
                      (lambda (chunk)
                        ;; Process each chunk through the callback
                        (funcall callback-fn chunk writer))
                      :temperature temperature)))
         
         ;; Send completion or error event
         (if (eq result :error)
             (funcall writer (format-sse-error "LLM service error"))
             (funcall writer 
                     (format-sse-data 
                      (list (cons :done t)
                            (cons :type "done"))))))))))

(defun create-htmx-streaming-handler (prompt-fn html-wrapper-fn &key (temperature 0.7) language)
  "Create a streaming handler for HTMX SSE extension that sends HTML fragments.
   
   This function creates streaming handlers specifically for HTMX SSE extension.
   It sends HTML fragments as SSE events with proper event names that HTMX
   can process and swap into the DOM.
   
   HTMX SSE Extension Integration:
   --------------------------------
   HTMX SSE extension expects:
   1. Named events (event: message, event: done, event: error)
   2. HTML content in the data field
   3. Events are swapped into target elements based on sse-swap attribute
   
   HTML Attributes:
   ----------------
   <div hx-ext=\"sse\"
        sse-connect=\"/api/feature-stream\"
        sse-swap=\"message\"
        hx-target=\"#result\">
   </div>
   
   This will:
   - Connect to /api/feature-stream
   - Listen for 'message' events
   - Swap HTML content into #result
   
   Parameters:
   -----------
   - prompt-fn: Function that returns the prompt string
                Example: (lambda () (build-spell-generator-prompt language))
   
   - html-wrapper-fn: Function that wraps content chunks in HTML
                      Receives (chunk) and returns HTML string
                      Example: (lambda (chunk) 
                                (format nil \"<span>~A</span>\" (escape-html chunk)))
   
   - temperature: LLM temperature parameter (default: 0.7)
   
   - language: Language code for error messages (default: \"en\")
   
   Returns:
   --------
   - SSE response function compatible with create-sse-response
   
   Behavior:
   ---------
   1. Calls prompt-fn to get the prompt
   2. Initiates streaming LLM call
   3. For each chunk:
      - Wraps chunk in HTML using html-wrapper-fn
      - Sends as 'message' event
   4. On completion, sends 'done' event
   5. On error, sends 'error' event with HTML error alert
   
   Example Usage:
   --------------
   (create-htmx-streaming-handler
     (lambda () (build-spell-generator-prompt \"en\"))
     (lambda (chunk) (format nil \"<span>~A</span>\" (escape-html chunk)))
     :temperature 0.9
     :language \"en\")"
  (create-sse-response
   (lambda (writer)
     ;; Get the prompt
     (let ((prompt (funcall prompt-fn))
           (lang (or language "en")))
       ;; Call LLM streaming with the callback
       (let ((result (call-llm-streaming 
                      prompt
                      (lambda (chunk)
                        ;; Wrap chunk in HTML and send as 'message' event
                        (let ((html-chunk (funcall html-wrapper-fn chunk)))
                          (funcall writer (format-sse-event "message" html-chunk))))
                      :temperature temperature)))
         
         ;; Send completion or error event
         (if (eq result :error)
             ;; Send HTML error alert as 'error' event
             (funcall writer (format-sse-html-error "LLM service error" lang))
             ;; Send empty 'done' event to signal completion
             (funcall writer (format-sse-event "done" ""))))))))

(defun create-htmx-streaming-handler-with-messages (messages html-wrapper-fn &key (temperature 0.7) system-prompt language on-complete)
  "Create a streaming handler for HTMX SSE extension with message-based LLM calls.
   
   This function is similar to create-htmx-streaming-handler but uses
   call-llm-with-messages-streaming instead of call-llm-streaming. This is
   needed for conversational features like trivia-bot that maintain message history.
   
   Parameters:
   -----------
   - messages: List of message plists with :role and :content
               Example: (list (list :role \"user\" :content \"Hello\")
                             (list :role \"assistant\" :content \"Hi\"))
   
   - html-wrapper-fn: Function that wraps content chunks in HTML
                      Receives (chunk) and returns HTML string
                      Example: (lambda (chunk) 
                                (format nil \"<span>~A</span>\" (escape-html chunk)))
   
   - temperature: LLM temperature parameter (default: 0.7)
   
   - system-prompt: System prompt for the LLM (optional)
   
   - language: Language code for error messages (default: \"en\")
   
   - on-complete: Optional callback function called with full response when streaming completes
                  Signature: (lambda (full-response) ...)
   
   Returns:
   --------
   - SSE response function compatible with create-sse-response
   
   Example Usage:
   --------------
   (create-htmx-streaming-handler-with-messages
     (list (list :role \"user\" :content \"What is Halloween?\"))
     (lambda (chunk) (format nil \"<span>~A</span>\" (escape-html chunk)))
     :temperature 0.75
     :system-prompt \"You are a trivia bot.\"
     :language \"en\"
     :on-complete (lambda (response) (save-to-history response)))"
  (create-sse-response
   (lambda (writer)
     (let ((lang (or language "en"))
           (accumulated-response (make-array 0 :element-type 'character 
                                              :fill-pointer 0 
                                              :adjustable t)))
       ;; Call LLM streaming with messages
       (let ((result (call-llm-with-messages-streaming 
                      messages
                      (lambda (chunk)
                        ;; Accumulate chunks for on-complete callback
                        (when on-complete
                          (loop for char across chunk
                                do (vector-push-extend char accumulated-response)))
                        ;; Wrap chunk in HTML and send as 'message' event
                        (let ((html-chunk (funcall html-wrapper-fn chunk)))
                          (funcall writer (format-sse-event "message" html-chunk))))
                      :temperature temperature
                      :system-prompt system-prompt)))
         
         ;; Send completion or error event
         (if (eq result :error)
             ;; Send HTML error alert as 'error' event
             (funcall writer (format-sse-html-error "LLM service error" lang))
             (progn
               ;; Call on-complete callback with full response
               (when (and on-complete (> (length accumulated-response) 0))
                 (funcall on-complete accumulated-response))
               ;; Send empty 'done' event to signal completion
               (funcall writer (format-sse-event "done" "")))))))))

(defun handle-streaming-error (condition language)
  "Handle errors in streaming context with appropriate error response.
   
   This function provides consistent error handling for streaming handlers.
   It logs the error and returns a properly formatted JSON error response
   suitable for streaming endpoints.
   
   Streaming errors are returned as JSON (not HTML) because streaming
   endpoints use Server-Sent Events which expect JSON payloads.
   
   Parameters:
   -----------
   - condition: Error condition object from handler-case
   - language: Language code for error message localization ('en' or 'ja')
   
   Returns:
   --------
   - HTTP response list with 500 status and JSON error body
     Format: (500 (:content-type \"application/json\") (json-error-string))
   
   Error Response Structure:
   -------------------------
   {
     \"error\": \"error message\",
     \"type\": \"error\"
   }
   
   The 'type' field allows clients to distinguish error events from
   content events in the SSE stream.
   
   Example Usage:
   --------------
   (handler-case
     (process-streaming-request params)
     (error (condition)
       (handle-streaming-error condition language)))"
  (log-error "streaming handler" condition)
  `(500 (:content-type "application/json")
    (,(cl-json:encode-json-to-string
        (list (cons :error (format nil "~A" condition))
              (cons :type "error"))))))
