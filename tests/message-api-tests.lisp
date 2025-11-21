(in-package :kabotan.tests)

;;; Message API Test Suite

(def-suite message-api-suite
  :in kabotan-suite
  :description "Message-based LLM API tests")

(in-suite message-api-suite)

;;; Message Role Validation Tests

(test validate-message-role-valid
  "Test that validate-message-role accepts valid roles"
  (is-true (kabotan::validate-message-role "system"))
  (is-true (kabotan::validate-message-role "user"))
  (is-true (kabotan::validate-message-role "assistant")))

(test validate-message-role-invalid
  "Test that validate-message-role rejects invalid roles"
  (is-false (kabotan::validate-message-role "admin"))
  (is-false (kabotan::validate-message-role "bot"))
  (is-false (kabotan::validate-message-role ""))
  (is-false (kabotan::validate-message-role nil)))

;;; Message to JSON Conversion Tests

(test messages-to-json-array-valid-single
  "Test that messages-to-json-array converts single message correctly"
  (let* ((messages (list (list :role "user" :content "Hello")))
         (result (kabotan::messages-to-json-array messages)))
    (is (listp result))
    (is (= 1 (length result)))
    (let ((first-msg (first result)))
      (is (string= "user" (cdr (assoc "role" first-msg :test #'string=))))
      (is (string= "Hello" (cdr (assoc "content" first-msg :test #'string=)))))))

(test messages-to-json-array-valid-multiple
  "Test that messages-to-json-array converts multiple messages correctly"
  (let* ((messages (list (list :role "user" :content "Hello")
                        (list :role "assistant" :content "Hi there!")
                        (list :role "user" :content "How are you?")))
         (result (kabotan::messages-to-json-array messages)))
    (is (listp result))
    (is (= 3 (length result)))
    (let ((first-msg (first result))
          (second-msg (second result))
          (third-msg (third result)))
      (is (string= "user" (cdr (assoc "role" first-msg :test #'string=))))
      (is (string= "Hello" (cdr (assoc "content" first-msg :test #'string=))))
      (is (string= "assistant" (cdr (assoc "role" second-msg :test #'string=))))
      (is (string= "Hi there!" (cdr (assoc "content" second-msg :test #'string=))))
      (is (string= "user" (cdr (assoc "role" third-msg :test #'string=))))
      (is (string= "How are you?" (cdr (assoc "content" third-msg :test #'string=)))))))

(test messages-to-json-array-with-system
  "Test that messages-to-json-array handles system messages"
  (let* ((messages (list (list :role "system" :content "You are a helpful assistant")
                        (list :role "user" :content "Hello")))
         (result (kabotan::messages-to-json-array messages)))
    (is (listp result))
    (is (= 2 (length result)))
    (let ((system-msg (first result)))
      (is (string= "system" (cdr (assoc "role" system-msg :test #'string=))))
      (is (string= "You are a helpful assistant" (cdr (assoc "content" system-msg :test #'string=)))))))

(test messages-to-json-array-invalid-role
  "Test that messages-to-json-array returns nil for invalid role"
  (let* ((messages (list (list :role "invalid" :content "Hello")))
         (result (kabotan::messages-to-json-array messages)))
    (is (null result))))

(test messages-to-json-array-missing-role
  "Test that messages-to-json-array returns nil for missing role"
  (let* ((messages (list (list :content "Hello")))
         (result (kabotan::messages-to-json-array messages)))
    (is (null result))))

(test messages-to-json-array-missing-content
  "Test that messages-to-json-array returns nil for missing content"
  (let* ((messages (list (list :role "user")))
         (result (kabotan::messages-to-json-array messages)))
    (is (null result))))

(test messages-to-json-array-empty-list
  "Test that messages-to-json-array handles empty list"
  (let* ((messages '())
         (result (kabotan::messages-to-json-array messages)))
    (is (listp result))
    (is (= 0 (length result)))))

;;; System Prompt Prepending Tests

(test call-llm-with-messages-no-system-prompt
  "Test that call-llm-with-messages works without system prompt"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((called-messages nil))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore max-tokens temperature timeout))
               (setf called-messages messages)
               "Response"))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages messages)))
                  (is (stringp response))
                  (is (string= "Response" response))
                  (is (= 1 (length called-messages)))
                  (is (string= "user" (getf (first called-messages) :role)))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

(test call-llm-with-messages-with-system-prompt
  "Test that call-llm-with-messages prepends system prompt"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((called-messages nil))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore max-tokens temperature timeout))
               (setf called-messages messages)
               "Response"))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages messages :system-prompt "You are helpful")))
                  (is (stringp response))
                  (is (string= "Response" response))
                  (is (= 2 (length called-messages)))
                  (is (string= "system" (getf (first called-messages) :role)))
                  (is (string= "You are helpful" (getf (first called-messages) :content)))
                  (is (string= "user" (getf (second called-messages) :role)))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

(test call-llm-with-messages-empty-system-prompt
  "Test that call-llm-with-messages ignores empty system prompt"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((called-messages nil))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore max-tokens temperature timeout))
               (setf called-messages messages)
               "Response"))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages messages :system-prompt "")))
                  (is (stringp response))
                  (is (= 1 (length called-messages)))
                  (is (string= "user" (getf (first called-messages) :role)))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

(test call-llm-with-messages-nil-system-prompt
  "Test that call-llm-with-messages ignores nil system prompt"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((called-messages nil))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore max-tokens temperature timeout))
               (setf called-messages messages)
               "Response"))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages messages :system-prompt nil)))
                  (is (stringp response))
                  (is (= 1 (length called-messages)))
                  (is (string= "user" (getf (first called-messages) :role)))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

;;; Message-based Retry Tests

(test call-llm-with-messages-retry-success-first-attempt
  "Test that call-llm-with-messages-retry returns response on first attempt"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((call-count 0))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore messages max-tokens temperature timeout))
               (incf call-count)
               "Success on first try"))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages-retry messages :max-retries 2)))
                  (is (stringp response))
                  (is (string= "Success on first try" response))
                  (is (= 1 call-count))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

(test call-llm-with-messages-retry-success-after-failure
  "Test that call-llm-with-messages-retry retries and succeeds"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((call-count 0))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore messages max-tokens temperature timeout))
               (incf call-count)
               (if (< call-count 2)
                   nil
                   "Success on second try")))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages-retry messages :max-retries 2)))
                  (is (stringp response))
                  (is (string= "Success on second try" response))
                  (is (= 2 call-count))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))

(test call-llm-with-messages-retry-exhausted
  "Test that call-llm-with-messages-retry returns nil after exhausting retries"
  ;; Mock the API function
  (let ((kabotan::*llm-api-function* nil))
    ;; Mock the internal API call
    (let ((call-count 0))
      (flet ((mock-api-call (messages &key max-tokens temperature timeout)
               (declare (ignore messages max-tokens temperature timeout))
               (incf call-count)
               nil))
        ;; Temporarily replace the function
        (let ((original-fn (symbol-function 'kabotan::call-openai-api-with-messages)))
          (unwind-protect
              (progn
                (setf (symbol-function 'kabotan::call-openai-api-with-messages) #'mock-api-call)
                (let* ((messages (list (list :role "user" :content "Hello")))
                       (response (call-llm-with-messages-retry messages :max-retries 2)))
                  (is (null response))
                  (is (= 2 call-count))))
            (setf (symbol-function 'kabotan::call-openai-api-with-messages) original-fn)))))))
