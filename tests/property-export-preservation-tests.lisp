(in-package :kabotan.tests)

;;; Property-Based Tests for Export Preservation
;;; Feature: 23-code-standards-compliance, Property 3: Exported functions are preserved
;;; Validates: Requirements 2.3

(def-suite property-export-preservation-suite
  :in kabotan-suite
  :description "Property-based tests to verify exported functions are preserved")

(in-suite property-export-preservation-suite)

;;; Helper function to get all exported symbols from a package

(defun get-exported-symbols (package-name)
  "Get list of all exported symbols from a package"
  (let ((package (find-package package-name))
        (symbols nil))
    (when package
      (do-external-symbols (sym package)
        (push sym symbols)))
    symbols))

;;; Helper function to check if a symbol is exported

(defun symbol-exported-p (symbol-name package-name)
  "Check if a symbol is exported from a package"
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (sym status)
          (find-symbol (string-upcase symbol-name) package)
        (declare (ignore sym))
        (eq status :external)))))

;;; Expected exported functions from kabotan package
;;; This list represents the baseline of exported functions before refactoring

(defparameter *expected-exports*
  '(;; Main entry point
    main
    
    ;; Error handling
    handle-api-error
    format-error-response
    log-error
    *suppress-error-logs*
    
    ;; Response formatting
    format-html-response
    format-spell-generator-response
    
    ;; Language handling
    get-ui-text
    detect-browser-language
    
    ;; LLM service
    call-llm
    call-llm-with-retry
    call-llm-with-messages
    call-llm-with-messages-retry
    
    ;; API setup
    setup-halloween-api
    get-param
    
    ;; Streaming utilities
    parse-sse-chunk
    format-sse-data
    format-sse-error
    create-sse-response
    handle-monster-diagnostic-streaming
    handle-story-generator-streaming
    handle-character-chat-streaming
    handle-trivia-bot-streaming
    handle-spell-generator-streaming)
  "List of expected exported symbols from kabotan package (updated after dead code removal)")

;;; Test: All expected exports are preserved

(test property-exports-preserved
  "Property 3: All expected exported functions should be preserved after refactoring"
  (let ((test-passed t)
        (missing-exports nil))
    
    ;; Property: All expected exports should still be exported
    (dolist (expected-symbol *expected-exports*)
      (unless (symbol-exported-p (symbol-name expected-symbol) :kabotan)
        (format t "~&FAILED: Expected export ~A is not exported~%" expected-symbol)
        (push expected-symbol missing-exports)
        (setf test-passed nil)))
    
    (if test-passed
        (format t "~&All ~D expected exports are preserved~%" (length *expected-exports*))
        (format t "~&Missing exports: ~{~A~^, ~}~%" missing-exports))
    
    (is-true test-passed "All expected exported functions are preserved")))

;;; Test: No unexpected exports removed

(test property-no-exports-removed
  "Property 3: No exports should be removed during refactoring"
  (let ((current-exports (get-exported-symbols :kabotan))
        (test-passed t))
    
    ;; Property: Current export count should be >= expected count
    (when (< (length current-exports) (length *expected-exports*))
      (format t "~&WARNING: Current exports (~D) < expected exports (~D)~%"
              (length current-exports) (length *expected-exports*))
      (setf test-passed nil))
    
    (format t "~&Current exports: ~D, Expected: ~D~%"
            (length current-exports) (length *expected-exports*))
    
    (is-true test-passed "Export count is maintained or increased")))

;;; Note: Legacy validation function tests removed as those functions
;;; have been removed from the codebase during dead code cleanup

;;; Test: All LLM service functions are exported

(test property-llm-service-exports-preserved
  "Property 3: All LLM service functions should be exported"
  (let ((test-passed t)
        (llm-functions '(call-llm
                        call-llm-with-retry
                        call-llm-with-messages
                        call-llm-with-messages-retry)))
    
    (dolist (func llm-functions)
      (unless (symbol-exported-p (symbol-name func) :kabotan)
        (format t "~&FAILED: LLM service function ~A is not exported~%" func)
        (setf test-passed nil)))
    
    (is-true test-passed "All LLM service functions are exported")))

;;; Test: All streaming functions are exported

(test property-streaming-exports-preserved
  "Property 3: All streaming functions should be exported"
  (let ((test-passed t)
        (streaming-functions '(parse-sse-chunk
                              format-sse-data
                              format-sse-error
                              create-sse-response
                              handle-monster-diagnostic-streaming
                              handle-story-generator-streaming
                              handle-character-chat-streaming
                              handle-trivia-bot-streaming
                              handle-spell-generator-streaming)))
    
    (dolist (func streaming-functions)
      (unless (symbol-exported-p (symbol-name func) :kabotan)
        (format t "~&FAILED: Streaming function ~A is not exported~%" func)
        (setf test-passed nil)))
    
    (is-true test-passed "All streaming functions are exported")))
