(in-package :kabotan)

;;; LLM Client
;;; Core API client functions for OpenAI-compatible LLM services

;;; Configuration variables
(defvar *model* "gpt-oss-120b"
  "The LLM model to use for API requests.")

(defvar *model-host* "http://localhost:8080/v1/chat/completions"
  "The API endpoint URL for LLM requests.")

;;; Dynamic variable for testing - allows tests to override the API call function
(defvar *llm-api-function* nil
  "Function to call for LLM API requests. When nil, uses the default implementation.
   Tests can bind this to a mock function.")

(defvar *llm-api-messages-function* nil
  "Function to call for LLM API requests with message arrays. When nil, uses the default implementation.
   Tests can bind this to a mock function.")

(defun call-openai-api (prompt &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the OpenAI-compatible API directly using dexador.
   
   Parameters:
   - prompt: The text prompt to send to the LLM
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   - timeout: Maximum time in seconds to wait for response (default: 360, ~6 minutes)
   
   Returns:
   - Response text string on success
   - nil on failure"
  (let* ((api-key (or (uiop:getenv "OPENAI_API_KEY") 
                     (uiop:getenv "OPENAI_KEY") 
                     "dummy"))
         (model (or (uiop:getenv "OPENAI_MODEL") "gpt-oss-120b"))
         (host (or (uiop:getenv "OPENAI_HOST") 
                  "http://localhost:8080/v1/chat/completions"))
         (start-time (get-internal-real-time)))
    
    ;; Log request details
    (log-llm-request host
                     model
                     (list :length (length prompt))
                     (list :max-tokens max-tokens :temperature temperature))
    
    (handler-case
        (let* ((request-body (cl-json:encode-json-to-string
                              `(("model" . ,model)
                                ("messages" . ((("role" . "user")
                                               ("content" . ,prompt))))
                                ("max_tokens" . ,max-tokens)
                                ("temperature" . ,temperature))))
               (headers `(("Content-Type" . "application/json")
                         ("Authorization" . ,(format nil "Bearer ~A" api-key)))))
          
          ;; Make HTTP POST request with timeout
          (multiple-value-bind (body status-code)
              (dex:post host
                       :headers headers
                       :content request-body
                       :read-timeout timeout
                       :connect-timeout 10)
            
            ;; Calculate response time
            (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                  internal-time-units-per-second)))
              
              ;; Check status code
              (unless (= status-code 200)
                (log-llm-error :http-error
                              (format nil "HTTP status ~A" status-code)
                              (list :status status-code))
                (log-error "LLM API error" 
                          (format nil "HTTP status ~A: ~A" status-code body))
                (return-from call-openai-api nil))
              
              ;; Parse JSON response
              (let* ((response-json (cl-json:decode-json-from-string body))
                     (choices (cdr (assoc :choices response-json)))
                     (first-choice (car choices))
                     (message (cdr (assoc :message first-choice)))
                     (content (cdr (assoc :content message)))
                     (reasoning-content (cdr (assoc :reasoning--content message))))
                
                ;; Debug logging
                (format *error-output* "[DEBUG] Response body (first 500 chars): ~A~%" 
                       (subseq body 0 (min 500 (length body))))
                (format *error-output* "[DEBUG] Content: ~A~%" content)
                (format *error-output* "[DEBUG] Reasoning content length: ~A~%" 
                       (if reasoning-content (length reasoning-content) 0))
                (force-output *error-output*)
                
                ;; Use content if available, otherwise fall back to reasoning_content
                (let ((final-content (cond
                                      ((and content (stringp content) (> (length content) 0))
                                       content)
                                      ((and reasoning-content (stringp reasoning-content) (> (length reasoning-content) 0))
                                       (format *error-output* "[DEBUG] Using reasoning_content as fallback~%")
                                       (force-output *error-output*)
                                       reasoning-content)
                                      (t nil))))
                  
                  ;; Log response details
                  (log-llm-response status-code
                                   (list :length (if final-content
                                                     (length final-content)
                                                     0))
                                   elapsed-time)
                  
                  ;; Return final content
                  final-content)))))
      
      (error (condition)
        (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))
          ;; Determine error type based on condition
          (let ((error-type (cond
                             ((search "timeout" (format nil "~A" condition) :test #'char-equal)
                              :timeout)
                             ((search "http" (format nil "~A" condition) :test #'char-equal)
                              :http-error)
                             (t :unknown))))
            (log-llm-error error-type
                          (format nil "~A" condition)
                          (list :elapsed elapsed-time))
            (log-error "LLM API error" 
                      (format nil "~A" condition))
            nil))))))

(defun call-openai-api-with-messages (messages &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the OpenAI-compatible API with a message array using dexador.
   
   Parameters:
   - messages: List of message plists with :role and :content keys
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   - timeout: Maximum time in seconds to wait for response (default: 360, ~6 minutes)
   
   Returns:
   - Response text string on success
   - nil on failure"
  (let* ((api-key (or (uiop:getenv "OPENAI_API_KEY") 
                     (uiop:getenv "OPENAI_KEY") 
                     "dummy"))
         (model (or (uiop:getenv "OPENAI_MODEL") "gpt-oss-120b"))
         (host (or (uiop:getenv "OPENAI_HOST") 
                  "http://localhost:8080/v1/chat/completions"))
         (start-time (get-internal-real-time)))
    
    ;; Convert messages to JSON-compatible format
    (let ((json-messages (messages-to-json-array messages)))
      (unless json-messages
        (log-error "Message conversion failed" "Could not convert messages to JSON format")
        (return-from call-openai-api-with-messages nil))
      
      ;; Log request details
      (log-llm-request host
                       model
                       (list :message-count (length messages))
                       (list :max-tokens max-tokens :temperature temperature))
      
      (handler-case
          (let* ((request-body (cl-json:encode-json-to-string
                                `(("model" . ,model)
                                  ("messages" . ,json-messages)
                                  ("max_tokens" . ,max-tokens)
                                  ("temperature" . ,temperature))))
                 (headers `(("Content-Type" . "application/json")
                           ("Authorization" . ,(format nil "Bearer ~A" api-key)))))
            
            ;; Make HTTP POST request with timeout
            (multiple-value-bind (body status-code)
                (dex:post host
                         :headers headers
                         :content request-body
                         :read-timeout timeout
                         :connect-timeout 10)
              
              ;; Calculate response time
              (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                    internal-time-units-per-second)))
                
                ;; Check status code
                (unless (= status-code 200)
                  (log-llm-error :http-error
                                (format nil "HTTP status ~A" status-code)
                                (list :status status-code))
                  (log-error "LLM API error" 
                            (format nil "HTTP status ~A: ~A" status-code body))
                  (return-from call-openai-api-with-messages nil))
                
                ;; Parse JSON response
                (let* ((response-json (cl-json:decode-json-from-string body))
                       (choices (cdr (assoc :choices response-json)))
                       (first-choice (car choices))
                       (message (cdr (assoc :message first-choice)))
                       (content (cdr (assoc :content message)))
                       (reasoning-content (cdr (assoc :reasoning--content message))))
                  
                  ;; Use content if available, otherwise fall back to reasoning_content
                  (let ((final-content (cond
                                        ((and content (stringp content) (> (length content) 0))
                                         content)
                                        ((and reasoning-content (stringp reasoning-content) (> (length reasoning-content) 0))
                                         reasoning-content)
                                        (t nil))))
                    
                    ;; Log response details
                    (log-llm-response status-code
                                     (list :length (if final-content
                                                       (length final-content)
                                                       0))
                                     elapsed-time)
                    
                    ;; Return final content
                    final-content)))))
        
        (error (condition)
          (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                                internal-time-units-per-second)))
            ;; Determine error type based on condition
            (let ((error-type (cond
                               ((search "timeout" (format nil "~A" condition) :test #'char-equal)
                                :timeout)
                               ((search "http" (format nil "~A" condition) :test #'char-equal)
                                :http-error)
                               (t :unknown))))
              (log-llm-error error-type
                            (format nil "~A" condition)
                            (list :elapsed elapsed-time))
              (log-error "LLM API error" 
                        (format nil "~A" condition))
              nil)))))))
