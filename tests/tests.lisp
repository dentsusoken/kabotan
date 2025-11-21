(in-package :kabotan.tests)

;;; Main Test Suite Definition

(def-suite kabotan-suite
  :description "Main test suite for Kabotan")

;;; Test Runner

(defun run-tests ()
  "全テストを実行し、成功なら T, 失敗なら NIL を返す。"
  ;; Suppress error logs during testing to reduce noise
  ;; Set KABOTAN_TEST_VERBOSE=1 environment variable to see all logs
  (let ((kabotan::*suppress-error-logs* 
         (not (equal (uiop:getenv "KABOTAN_TEST_VERBOSE") "1"))))
    (when kabotan::*suppress-error-logs*
      (format t "~%[INFO] Running tests with error log suppression enabled~%")
      (format t "[INFO] Set KABOTAN_TEST_VERBOSE=1 to see all error logs~%~%"))
    (run! 'kabotan-suite)))
