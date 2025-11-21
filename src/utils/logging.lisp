(in-package :kabotan)

;;; Logging Utility Module
;;; Provides structured logging for LLM service operations

(defun format-iso8601-timestamp (universal-time)
  "Format universal time as ISO 8601 timestamp (YYYY-MM-DDTHH:MM:SS)"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

(defun log-message (level component message)
  "Write a structured log message to *error-output*
   
   Parameters:
   - level: Log level (:info, :warn, :error)
   - component: Component name (:llm-request, :llm-response, :llm-error, :llm-retry)
   - message: Log message string"
  (let ((timestamp (format-iso8601-timestamp (get-universal-time)))
        (level-str (string-upcase (symbol-name level)))
        (component-str (string-upcase (symbol-name component))))
    (format *error-output* "[~A] [~A] [~A] ~A~%"
            timestamp level-str component-str message)
    (force-output *error-output*)))

(defun log-llm-request (endpoint model prompt-info config)
  "Log LLM API request details
   
   Parameters:
   - endpoint: API endpoint URL string
   - model: Model name string
   - prompt-info: Plist with :length (character count) or :messages-count
   - config: Plist with :max-tokens and :temperature"
  (let* ((prompt-desc (if (getf prompt-info :length)
                          (format nil "Prompt: ~A chars" (getf prompt-info :length))
                          (format nil "Messages: ~A" (getf prompt-info :messages-count))))
         (tokens (getf config :max-tokens))
         (temp (getf config :temperature))
         (message (format nil "Endpoint: ~A, Model: ~A, ~A, Tokens: ~A, Temp: ~A"
                         endpoint model prompt-desc tokens temp)))
    (log-message :info :llm-request message)))

(defun log-llm-response (status-code response-info timing)
  "Log LLM API response details
   
   Parameters:
   - status-code: HTTP status code
   - response-info: Plist with :length (character count)
   - timing: Response time in seconds"
  (let* ((response-length (getf response-info :length))
         (message (format nil "Status: ~A, Response: ~A chars, Time: ~,1Fs"
                         status-code response-length timing)))
    (log-message :info :llm-response message)))

(defun log-llm-error (error-type error-message context)
  "Log LLM API errors with context
   
   Parameters:
   - error-type: Error type keyword (:timeout, :http-error, :unknown)
   - error-message: Error description string
   - context: Plist with additional context (optional)"
  (let* ((context-str (if context
                          (format nil ", Context: ~{~A: ~A~^, ~}" 
                                 (loop for (key value) on context by #'cddr
                                       collect (string-downcase (symbol-name key))
                                       collect value))
                          ""))
         (message (format nil "~A~A" error-message context-str)))
    (log-message :error :llm-error message)))

(defun log-llm-retry (attempt max-attempts backoff-seconds reason)
  "Log retry attempts
   
   Parameters:
   - attempt: Current attempt number
   - max-attempts: Maximum number of attempts
   - backoff-seconds: Backoff duration in seconds
   - reason: Reason for retry (e.g., 'timeout', 'http-error')"
  (let ((message (format nil "Attempt ~A/~A, Backoff: ~As, Reason: ~A"
                        attempt max-attempts backoff-seconds reason)))
    (log-message :warn :llm-retry message)))

(defun log-debug (component message)
  "Log debug information
   
   Parameters:
   - component: Component name keyword
   - message: Debug message string"
  (log-message :debug component message))
