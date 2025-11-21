(in-package :kabotan.tests)

;;; LLM Service Tests

(def-suite llm-service-suite
  :in kabotan-suite
  :description "LLM service tests")

(in-suite llm-service-suite)

(test call-llm-with-mock-success
  "Test that call-llm returns response on success"
  ;; Mock the LLM API function
  (let ((kabotan::*llm-api-function*
          (lambda (prompt max-tokens temperature timeout)
            (declare (ignore prompt max-tokens temperature timeout))
            "This is a test response from the LLM")))
    (let ((response (call-llm "test prompt")))
      (is (stringp response))
      (is (string= "This is a test response from the LLM" response)))))

(test call-llm-with-mock-error
  "Test that call-llm returns nil on error"
  ;; Mock the LLM API function to return nil (simulating error)
  (let ((kabotan::*llm-api-function*
          (lambda (prompt max-tokens temperature timeout)
            (declare (ignore prompt max-tokens temperature timeout))
            nil)))
    (let ((response (call-llm "test prompt")))
      (is (null response)))))

(test call-llm-with-retry-success-first-attempt
  "Test that call-llm-with-retry returns response on first attempt"
  ;; Mock the LLM API function
  (let ((kabotan::*llm-api-function*
          (lambda (prompt max-tokens temperature timeout)
            (declare (ignore prompt max-tokens temperature timeout))
            "Success on first try")))
    (let ((response (call-llm-with-retry "test prompt" :max-retries 2)))
      (is (stringp response))
      (is (string= "Success on first try" response)))))

(test call-llm-with-retry-success-after-failure
  "Test that call-llm-with-retry retries and succeeds"
  ;; Mock the LLM API function to fail once then succeed
  (let ((attempt-count 0))
    (let ((kabotan::*llm-api-function*
            (lambda (prompt max-tokens temperature timeout)
              (declare (ignore prompt max-tokens temperature timeout))
              (incf attempt-count)
              (if (< attempt-count 2)
                  nil  ; Return nil to simulate failure
                  "Success on second try"))))
      (let ((response (call-llm-with-retry "test prompt" :max-retries 2)))
        (is (stringp response))
        (is (string= "Success on second try" response))
        (is (= 2 attempt-count))))))

(test call-llm-with-retry-exhausted
  "Test that call-llm-with-retry returns nil after exhausting retries"
  ;; Mock the LLM API function to always fail
  (let ((attempt-count 0))
    (let ((kabotan::*llm-api-function*
            (lambda (prompt max-tokens temperature timeout)
              (declare (ignore prompt max-tokens temperature timeout))
              (incf attempt-count)
              nil)))  ; Always return nil to simulate failure
      (let ((response (call-llm-with-retry "test prompt" :max-retries 2)))
        (is (null response))
        (is (= 2 attempt-count))))))
