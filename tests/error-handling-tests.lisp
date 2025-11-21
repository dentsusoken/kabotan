(in-package :kabotan.tests)

;;; Error Handling Tests

(def-suite error-handling-suite
  :in kabotan-suite
  :description "Error handling utility tests")

(in-suite error-handling-suite)

(test format-error-response-japanese
  "Test that format-error-response produces correct HTML for Japanese"
  ;; Test with custom message
  (let ((result (format-error-response (list :error-type "test" :message "カスタムエラー") "ja")))
    (is (search "alert alert-error" result))
    (is (search "カスタムエラー" result)))
  ;; Test with default message (no :message key)
  (let ((result (format-error-response (list :error-type "test") "ja")))
    (is (search "alert alert-error" result))
    (is (search "エラーが発生しました" result))))

(test format-error-response-english
  "Test that format-error-response produces correct HTML for English"
  ;; Test with custom message
  (let ((result (format-error-response (list :error-type "test" :message "Custom error") "en")))
    (is (search "alert alert-error" result))
    (is (search "Custom error" result)))
  ;; Test with default message (no :message key)
  (let ((result (format-error-response (list :error-type "test") "en")))
    (is (search "alert alert-error" result))
    (is (search "An error occurred" result))))
