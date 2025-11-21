(in-package :kabotan.tests)

;;; Logging Tests

(def-suite logging-suite
  :in kabotan-suite
  :description "Logging utility tests")

(in-suite logging-suite)

;;; Helper function to capture log output
(defun capture-log-output (fn)
  "Capture output written to *error-output* during function execution"
  (with-output-to-string (*error-output*)
    (funcall fn)))

;;; Timestamp Formatting Tests

(test format-iso8601-timestamp-basic
  "Test that format-iso8601-timestamp produces correct ISO 8601 format"
  (let* ((universal-time (encode-universal-time 45 30 14 13 11 2024))
         (timestamp (kabotan::format-iso8601-timestamp universal-time)))
    (is (stringp timestamp))
    (is (string= "2024-11-13T14:30:45" timestamp))))

(test format-iso8601-timestamp-padding
  "Test that format-iso8601-timestamp pads single digits with zeros"
  (let* ((universal-time (encode-universal-time 5 9 8 7 6 2024))
         (timestamp (kabotan::format-iso8601-timestamp universal-time)))
    (is (string= "2024-06-07T08:09:05" timestamp))))

;;; Log Message Structure Tests

(test log-message-format
  "Test that log-message produces correct structured format"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-message :info :test-component "Test message")))))
    (is (search "[INFO]" output))
    (is (search "[TEST-COMPONENT]" output))
    (is (search "Test message" output))
    ;; Check timestamp format (YYYY-MM-DD)
    (is (cl-ppcre:scan "\\[\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\]" output))))

(test log-message-severity-levels
  "Test that log-message handles different severity levels"
  (let ((info-output (capture-log-output
                      (lambda ()
                        (kabotan::log-message :info :component "Info message"))))
        (warn-output (capture-log-output
                      (lambda ()
                        (kabotan::log-message :warn :component "Warning message"))))
        (error-output (capture-log-output
                       (lambda ()
                         (kabotan::log-message :error :component "Error message")))))
    (is (search "[INFO]" info-output))
    (is (search "[WARN]" warn-output))
    (is (search "[ERROR]" error-output))))

;;; LLM Request Logging Tests

(test log-llm-request-with-prompt-length
  "Test that log-llm-request logs prompt-based requests correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-request
                    "http://example.com/api"
                    "gemma3-12b"
                    (list :length 150)
                    (list :max-tokens 1000 :temperature 0.7))))))
    (is (search "[INFO]" output))
    (is (search "[LLM-REQUEST]" output))
    (is (search "Endpoint: http://example.com/api" output))
    (is (search "Model: gemma3-12b" output))
    (is (search "Prompt: 150 chars" output))
    (is (search "Tokens: 1000" output))
    (is (search "Temp: 0.7" output))))

(test log-llm-request-with-messages-count
  "Test that log-llm-request logs message-based requests correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-request
                    "http://example.com/api"
                    "gemma3-12b"
                    (list :messages-count 5)
                    (list :max-tokens 2000 :temperature 0.8))))))
    (is (search "[INFO]" output))
    (is (search "[LLM-REQUEST]" output))
    (is (search "Messages: 5" output))
    (is (search "Tokens: 2000" output))
    (is (search "Temp: 0.8" output))))

;;; LLM Response Logging Tests

(test log-llm-response-success
  "Test that log-llm-response logs successful responses correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-response
                    200
                    (list :length 450)
                    2.3)))))
    (is (search "[INFO]" output))
    (is (search "[LLM-RESPONSE]" output))
    (is (search "Status: 200" output))
    (is (search "Response: 450 chars" output))
    (is (search "Time: 2.3s" output))))

(test log-llm-response-timing-format
  "Test that log-llm-response formats timing with one decimal place"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-response
                    200
                    (list :length 100)
                    1.567)))))
    (is (search "Time: 1.6s" output))))

;;; LLM Error Logging Tests

(test log-llm-error-timeout
  "Test that log-llm-error logs timeout errors correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-error
                    :timeout
                    "Request timeout after 30s"
                    (list :timeout 30 :messages-count 3))))))
    (is (search "[ERROR]" output))
    (is (search "[LLM-ERROR]" output))
    (is (search "Request timeout after 30s" output))
    (is (search "Context: timeout: 30, messages-count: 3" output))))

(test log-llm-error-http-error
  "Test that log-llm-error logs HTTP errors correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-error
                    :http-error
                    "HTTP 500: Internal Server Error"
                    (list :status 500))))))
    (is (search "[ERROR]" output))
    (is (search "[LLM-ERROR]" output))
    (is (search "HTTP 500: Internal Server Error" output))
    (is (search "Context: status: 500" output))))

(test log-llm-error-without-context
  "Test that log-llm-error handles nil context correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-error
                    :unknown
                    "Unknown error occurred"
                    nil)))))
    (is (search "[ERROR]" output))
    (is (search "[LLM-ERROR]" output))
    (is (search "Unknown error occurred" output))
    (is (not (search "Context:" output)))))

;;; LLM Retry Logging Tests

(test log-llm-retry-basic
  "Test that log-llm-retry logs retry attempts correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-llm-retry 1 2 1 "timeout")))))
    (is (search "[WARN]" output))
    (is (search "[LLM-RETRY]" output))
    (is (search "Attempt 1/2" output))
    (is (search "Backoff: 1s" output))
    (is (search "Reason: timeout" output))))

(test log-llm-retry-multiple-attempts
  "Test that log-llm-retry logs multiple retry attempts"
  (let ((output1 (capture-log-output
                  (lambda ()
                    (kabotan::log-llm-retry 1 3 1 "http-error"))))
        (output2 (capture-log-output
                  (lambda ()
                    (kabotan::log-llm-retry 2 3 2 "http-error")))))
    (is (search "Attempt 1/3" output1))
    (is (search "Backoff: 1s" output1))
    (is (search "Attempt 2/3" output2))
    (is (search "Backoff: 2s" output2))))

;;; Debug Logging Tests

(test log-debug-basic
  "Test that log-debug logs debug messages correctly"
  (let ((output (capture-log-output
                 (lambda ()
                   (kabotan::log-debug :test-component "Debug information")))))
    (is (search "[DEBUG]" output))
    (is (search "[TEST-COMPONENT]" output))
    (is (search "Debug information" output))))
