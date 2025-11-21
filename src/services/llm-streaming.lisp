(in-package :kabotan)

;;; LLM Streaming Functions
;;; Streaming communication with OpenAI-compatible LLM APIs
;;;
;;; Architecture: Client → Backend → LLM API (streaming) → Callback → Client
;;; Callbacks receive content chunks and should execute quickly
;;; Errors include context about partial responses for debugging

;;; Streaming configuration variables

(defvar *llm-streaming-function* nil
  "Function to call for streaming LLM API requests. When nil, uses the default implementation.
   Tests can bind this to a mock function that simulates streaming behavior.
   
   Mock function signature:
   (lambda (prompt callback max-tokens temperature timeout) ...)")

(defvar *llm-streaming-messages-function* nil
  "Function to call for streaming LLM API requests with message arrays. When nil, uses the default implementation.
   Tests can bind this to a mock function that simulates streaming behavior.
   
   Mock function signature:
   (lambda (messages callback max-tokens temperature timeout system-prompt) ...)")

(defun call-openai-api-streaming (prompt callback &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the OpenAI-compatible API with streaming enabled.
   Reads SSE stream line-by-line, parses chunks, invokes callback for each content piece.
   Returns :success or :error. Tracks chunk-count and total-chars for debugging."
  (let* ((api-key (or (uiop:getenv "OPENAI_API_KEY") 
                     (uiop:getenv "OPENAI_KEY") 
                     "dummy"))
         (model (or (uiop:getenv "OPENAI_MODEL") "gpt-oss-120b"))
         (host (or (uiop:getenv "OPENAI_HOST") 
                  "http://localhost:8080/v1/chat/completions"))
         (start-time (get-internal-real-time))
         (stream nil)
         (chunk-count 0)
         (total-chars 0))
    
    ;; Log request details
    (log-llm-request host
                     model
                     (list :length (length prompt) :streaming t)
                     (list :max-tokens max-tokens :temperature temperature))
    
    (handler-case
        (progn
          (let* ((request-body (cl-json:encode-json-to-string
                                `(("model" . ,model)
                                  ("messages" . ((("role" . "user")
                                                 ("content" . ,prompt))))
                                  ("max_tokens" . ,max-tokens)
                                  ("temperature" . ,temperature)
                                  ("stream" . t))))
                 (headers `(("Content-Type" . "application/json")
                           ("Authorization" . ,(format nil "Bearer ~A" api-key)))))
            
            ;; Make HTTP POST request with :want-stream t to get raw stream
            (setf stream (dex:post host
                                  :headers headers
                                  :content request-body
                                  :want-stream t
                                  :read-timeout timeout
                                  :connect-timeout 10))
            
            ;; Read stream line-by-line and parse SSE format
            ;; Note: Dexador already returns a properly decoded stream
            (loop for line = (read-line stream nil nil)
                  while line
                  do (progn
                       ;; Parse SSE chunk and extract content
                       (let ((content (parse-sse-chunk line)))
                         (when content
                           ;; Track streaming statistics
                           (incf chunk-count)
                           (incf total-chars (length content))
                           ;; Invoke callback with content chunk
                           (funcall callback content)))))
            
            ;; Ensure stream is properly closed after successful completion
            (when stream
              (ignore-errors (close stream)))
            
            ;; Calculate response time
            (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                  internal-time-units-per-second)))
              
              ;; Log successful completion with streaming statistics
              (log-llm-response 200
                               (list :streaming t 
                                     :chunks chunk-count 
                                     :chars total-chars)
                               elapsed-time)
              
              ;; Return success
              :success)))
      
      (error (condition)
        ;; Ensure stream is closed on error
        (when stream
          (ignore-errors (close stream)))
        
        (let* ((elapsed-time (/ (- (get-internal-real-time) start-time)
                               internal-time-units-per-second))
               (condition-str (format nil "~A" condition)))
          ;; Check if this is a "Broken pipe" error (client disconnected)
          (if (search "Broken pipe" condition-str)
              ;; Client disconnected - this is normal, just log at debug level
              (progn
                (format t "[DEBUG] Client disconnected during LLM streaming (broken pipe, ~A chunks, ~A chars)~%" 
                        chunk-count total-chars)
                :error)
              ;; Other errors - log as errors
              (let ((error-type (cond
                                 ((search "timeout" condition-str :test #'char-equal)
                                  :timeout)
                                 ((search "connection" condition-str :test #'char-equal)
                                  :connection-error)
                                 ((search "http" condition-str :test #'char-equal)
                                  :http-error)
                                 (t :unknown))))
                ;; Log detailed error with partial response information
                (log-llm-error error-type
                              condition-str
                              (list :elapsed elapsed-time 
                                    :streaming t
                                    :chunks-received chunk-count
                                    :chars-received total-chars))
                (log-error "LLM streaming API error" 
                          (format nil "~A (received ~A chunks, ~A chars before failure)" 
                                  condition-str chunk-count total-chars))
                :error)))))))

(defun call-openai-api-with-messages-streaming (messages callback &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the OpenAI-compatible API with message array and streaming enabled.
   Validates messages, establishes streaming connection, invokes callback for each chunk.
   Returns :success or :error. Supports multi-turn conversations with system/user/assistant roles."
  (let* ((api-key (or (uiop:getenv "OPENAI_API_KEY") 
                     (uiop:getenv "OPENAI_KEY") 
                     "dummy"))
         (model (or (uiop:getenv "OPENAI_MODEL") "gpt-oss-120b"))
         (host (or (uiop:getenv "OPENAI_HOST") 
                  "http://localhost:8080/v1/chat/completions"))
         (start-time (get-internal-real-time))
         (stream nil)
         (chunk-count 0)
         (total-chars 0))
    
    ;; Convert messages to JSON-compatible format
    (let ((json-messages (messages-to-json-array messages)))
      (unless json-messages
        (log-error "Message conversion failed" "Could not convert messages to JSON format")
        (return-from call-openai-api-with-messages-streaming :error))
      
      ;; Log request details
      (log-llm-request host
                       model
                       (list :message-count (length messages) :streaming t)
                       (list :max-tokens max-tokens :temperature temperature))
      
      (handler-case
          (progn
            (let* ((request-body (cl-json:encode-json-to-string
                                  `(("model" . ,model)
                                    ("messages" . ,json-messages)
                                    ("max_tokens" . ,max-tokens)
                                    ("temperature" . ,temperature)
                                    ("stream" . t))))
                   (headers `(("Content-Type" . "application/json")
                             ("Authorization" . ,(format nil "Bearer ~A" api-key)))))
              
              ;; Make HTTP POST request with :want-stream t to get raw stream
              (setf stream (dex:post host
                                    :headers headers
                                    :content request-body
                                    :want-stream t
                                    :read-timeout timeout
                                    :connect-timeout 10))
              
              ;; Read stream line-by-line and parse SSE format
              ;; Note: Dexador already returns a properly decoded stream
              (loop for line = (read-line stream nil nil)
                    while line
                    do (progn
                         ;; Parse SSE chunk and extract content
                         (let ((content (parse-sse-chunk line)))
                           (when content
                             ;; Track streaming statistics
                             (incf chunk-count)
                             (incf total-chars (length content))
                             ;; Invoke callback with content chunk
                             (funcall callback content)))))
              
              ;; Ensure stream is properly closed after successful completion
              (when stream
                (ignore-errors (close stream)))
              
              ;; Calculate response time
              (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                    internal-time-units-per-second)))
                
                ;; Log successful completion with streaming statistics
                (log-llm-response 200
                                 (list :streaming t
                                       :chunks chunk-count
                                       :chars total-chars)
                                 elapsed-time)
                
                ;; Return success
                :success)))
        
        (error (condition)
          ;; Ensure stream is closed on error
          (when stream
            (ignore-errors (close stream)))
          
          (let* ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                 internal-time-units-per-second))
                 (condition-str (format nil "~A" condition)))
            ;; Check if this is a "Broken pipe" error (client disconnected)
            (if (search "Broken pipe" condition-str)
                ;; Client disconnected - this is normal, just log at debug level
                (progn
                  (format t "[DEBUG] Client disconnected during LLM streaming (broken pipe, ~A chunks, ~A chars)~%" 
                          chunk-count total-chars)
                  :error)
                ;; Other errors - log as errors
                (let ((error-type (cond
                                   ((search "timeout" condition-str :test #'char-equal)
                                    :timeout)
                                   ((search "connection" condition-str :test #'char-equal)
                                    :connection-error)
                                   ((search "http" condition-str :test #'char-equal)
                                    :http-error)
                                   (t :unknown))))
                  ;; Log detailed error with partial response information
                  (log-llm-error error-type
                                condition-str
                                (list :elapsed elapsed-time
                                      :streaming t
                                      :chunks-received chunk-count
                                      :chars-received total-chars))
                  (log-error "LLM streaming API error" 
                            (format nil "~A (received ~A chunks, ~A chars before failure)"
                                    condition-str chunk-count total-chars))
                  :error))))))))

(defun call-llm-streaming (prompt callback &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the LLM service with streaming enabled, invoking callback for each chunk.
   Wrapper function supporting test mocking via *llm-streaming-function*.
   Returns :success or :error."
  ;; Check if a test mock function is bound
  (if *llm-streaming-function*
      (funcall *llm-streaming-function* prompt callback max-tokens temperature timeout)
      ;; Otherwise use our custom streaming API call
      (call-openai-api-streaming prompt callback
                                :max-tokens max-tokens 
                                :temperature temperature 
                                :timeout timeout)))

(defun call-llm-with-messages-streaming (messages callback &key (max-tokens 10000) (temperature 0.7) (timeout 360) system-prompt)
  "Calls the LLM service with message array and streaming enabled.
   Supports conversational interactions with message history. System-prompt is prepended if provided.
   Wrapper function supporting test mocking via *llm-streaming-messages-function*.
   Returns :success or :error."
  ;; Check if a test mock function is bound
  (if *llm-streaming-messages-function*
      (funcall *llm-streaming-messages-function* 
               messages
               callback
               max-tokens 
               temperature 
               timeout
               system-prompt)
      ;; Otherwise use real API
      (progn
        ;; Prepend system prompt if provided
        (let ((final-messages (if (and system-prompt 
                                      (stringp system-prompt) 
                                      (> (length system-prompt) 0))
                                 (cons (list :role "system" :content system-prompt)
                                       messages)
                                 messages)))
          
          ;; Call API with messages
          (call-openai-api-with-messages-streaming final-messages callback
                                                  :max-tokens max-tokens
                                                  :temperature temperature
                                                  :timeout timeout)))))
