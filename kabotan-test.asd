(defsystem "kabotan-test"
  :description "Test system for kabotan"
  :author "ytsuyoshi <yamashita.tsuyoshi@dentsusoken.com>"
  :license "MIT"
  :depends-on ("kabotan" "fiveam" "cl-ppcre" "cl-quickcheck")
  :serial t
  :components
  ((:module "tests"
    :components
            ;; ========================================
            ;; Test Infrastructure
            ;; ========================================
            ;; Package definition must be loaded first
            ((:file "package")
             
             ;; Property test utilities must be loaded before property tests
             (:file "property-test-utils")     ; Utilities for property-based testing
             
             ;; Main test suite definition and runner
             (:file "tests")                   ; Main test runner
             
             ;; ========================================
             ;; Validation and Error Handling Tests
             ;; ========================================
             ;; Tests split from original tests.lisp
             (:file "error-handling-tests")    ; Error handling tests
             (:file "response-formatting-tests") ; Response formatting tests
             
             ;; ========================================
             ;; Service Tests
             ;; ========================================
             (:file "language-handler-tests")  ; Language handler tests
             (:file "prompt-builder-tests")    ; Prompt builder tests
             (:file "llm-service-tests")       ; LLM service tests (client, retry, messages)
             (:file "session-manager-tests")   ; Session management tests
             
             ;; ========================================
             ;; Utility Tests
             ;; ========================================
             (:file "logging-tests")           ; Logging functionality tests
             (:file "streaming-tests")         ; SSE protocol and streaming handler tests
             
             ;; ========================================
             ;; HTML Template Tests
             ;; ========================================
             ;; Tests split from original html-templates-tests.lisp
             (:file "html-forms-tests")        ; Form component tests
             (:file "html-features-tests")     ; Feature forms and chat component tests
             
             ;; ========================================
             ;; API Tests
             ;; ========================================
             (:file "message-api-tests")       ; Message API and validation tests
             (:file "feature-content-tests")   ; Feature content handler tests
             
             ;; ========================================
             ;; Property-Based Tests
             ;; ========================================
             ;; Tests that verify refactoring correctness properties
             (:file "property-behavior-preservation-tests")  ; Behavior preservation property
             (:file "property-test-coverage-tests")          ; Test coverage maintenance property
             (:file "property-export-preservation-tests")    ; Export preservation property
             )))
  :in-order-to ((test-op (load-op "kabotan-test"))))
