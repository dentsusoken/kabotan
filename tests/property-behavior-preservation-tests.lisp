(in-package :kabotan.tests)

;;; Property-Based Tests for Behavior Preservation
;;; Feature: 23-code-standards-compliance, Property 1: Refactoring preserves behavior
;;; Validates: Requirements 2.4, 3.4, 3.5, 4.4, 4.5, 5.3, 6.3, 8.5

(def-suite property-behavior-preservation-suite
  :in kabotan-suite
  :description "Property-based tests to verify refactoring preserves behavior")

(in-suite property-behavior-preservation-suite)

;;; Test: HTML escaping behavior is preserved

(test property-html-escaping-preservation
  "Property 1: For any string, HTML escaping produces consistent output"
  (let ((test-passed t))
    (dotimes (i *property-test-iterations*)
      (let* ((input (random-string 50))
             (result (kabotan::escape-html input)))
        ;; Property: Result should be a string
        (unless (stringp result)
          (format t "~&FAILED: HTML escaping should return string, got ~S~%" result)
          (setf test-passed nil)
          (return))
        ;; Property: Escaped string should not contain unescaped < or >
        (let ((has-lt (search "<" result))
              (has-gt (search ">" result)))
          (when has-lt
            (unless (search "&lt;" result)
              (format t "~&FAILED: Escaped string contains unescaped '<': ~S~%" result)
              (setf test-passed nil)
              (return)))
          (when has-gt
            (unless (search "&gt;" result)
              (format t "~&FAILED: Escaped string contains unescaped '>': ~S~%" result)
              (setf test-passed nil)
              (return))))))
    (is-true test-passed "HTML escaping preserves expected behavior across random inputs")))

;;; Note: Validation function tests removed as those functions have been removed from the codebase

;;; Test: SSE formatting behavior

(test property-sse-formatting-preservation
  "Property 1: For any data, SSE formatting produces valid SSE format"
  (let ((test-passed t))
    (dotimes (i *property-test-iterations*)
      (let* ((data (random-string 100))
             (result (kabotan::format-sse-data data)))
        ;; Property: SSE data should start with "data: "
        (unless (and (>= (length result) 6)
                     (string= "data: " result :end2 6))
          (format t "~&FAILED: SSE data should start with 'data: ', got ~S~%" result)
          (setf test-passed nil)
          (return))
        ;; Property: SSE data should end with double newline
        (unless (and (>= (length result) 2)
                     (string= (subseq result (- (length result) 2)) (format nil "~%~%")))
          (format t "~&FAILED: SSE data should end with double newline, got ~S~%" result)
          (setf test-passed nil)
          (return))))
    (is-true test-passed "SSE formatting preserves expected behavior across random inputs")))

;;; Test: Error response formatting

(test property-error-response-preservation
  "Property 1: For any error message, error response formatting is consistent"
  (let ((test-passed t))
    (dotimes (i *property-test-iterations*)
      (let* ((message (random-string 50))
             (error-info (list :error-type "test-error" :message message))
             (language (random-choice "en" "ja"))
             (result (kabotan::format-error-response error-info language)))
        ;; Property: Error response should be valid HTML
        (unless (and (search "<div" result)
                     (search "</div>" result))
          (format t "~&FAILED: Error response should be valid HTML, got ~S~%" result)
          (setf test-passed nil)
          (return))
        ;; Property: Error response should contain the message (possibly escaped)
        ;; We check that either the original or escaped message is present
        (let ((escaped-message (kabotan::escape-html message)))
          (unless (or (search message result)
                      (search escaped-message result))
            (format t "~&FAILED: Error response should contain message ~S or ~S, got ~S~%" 
                    message escaped-message result)
            (setf test-passed nil)
            (return)))))
    (is-true test-passed "Error response formatting preserves expected behavior across random inputs")))

;;; Note: Sanitize input, story style validation, and character validation tests removed
;;; as those functions have been removed from the codebase
