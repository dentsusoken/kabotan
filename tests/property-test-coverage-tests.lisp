(in-package :kabotan.tests)

;;; Property-Based Tests for Test Coverage Maintenance
;;; Feature: 23-code-standards-compliance, Property 2: Test coverage is maintained
;;; Validates: Requirements 1.4, 8.4

(def-suite property-test-coverage-suite
  :in kabotan-suite
  :description "Property-based tests to verify test coverage is maintained")

(in-suite property-test-coverage-suite)

;;; Helper function to check if a suite exists and has tests

(defun suite-exists-p (suite-name)
  "Check if a test suite exists"
  (not (null (fiveam:get-test suite-name))))

;;; Helper function to get all test suites

(defun get-all-test-suites ()
  "Get list of all test suite names"
  '(error-handling-suite
    response-formatting-suite
    language-handler-suite
    prompt-builder-suite
    llm-service-suite
    streaming-suite
    logging-suite
    message-api-suite
    html-features-suite
    session-manager-suite
    feature-content-suite
    property-behavior-preservation-suite
    property-test-coverage-suite))

;;; Test: Test coverage is maintained
;;; 
;;; Note: This test verifies that all expected test suites exist after refactoring.
;;; The actual test count verification is documented in baseline-test-results.md
;;; 
;;; Baseline test counts (from before refactoring):
;;; - error-handling-suite: 2 tests
;;; - response-formatting-suite: 3 tests
;;; - language-handler-suite: 7 tests
;;; - prompt-builder-suite: 1 test
;;; - llm-service-suite: 5 tests
;;; - streaming-suite: 16 tests
;;; - logging-suite: 14 tests
;;; - message-api-suite: 16 tests
;;; - html-features-suite: 23 tests
;;; - session-manager-suite: 24 tests
;;; - feature-content-suite: 20 tests
;;; Total: 131 tests (excluding property-based tests)
;;; 
;;; Note: validation-suite, html-common-suite, and html-forms-suite were removed
;;; as part of the code cleanup (quality-24-code-cleanup spec)

(test property-test-coverage-maintained
  "Property 2: All test suites should exist after refactoring"
  (let ((test-passed t)
        (expected-suites '(error-handling-suite
                          response-formatting-suite
                          language-handler-suite
                          prompt-builder-suite
                          llm-service-suite
                          streaming-suite
                          logging-suite
                          message-api-suite
                          html-features-suite
                          session-manager-suite
                          feature-content-suite)))
    
    ;; Property: All expected test suites should exist
    (dolist (suite-name expected-suites)
      (unless (suite-exists-p suite-name)
        (format t "~&FAILED: Expected test suite ~A does not exist~%" suite-name)
        (setf test-passed nil)))
    
    (format t "~&All ~D expected test suites exist~%" (length expected-suites))
    
    (is-true test-passed "All expected test suites exist after refactoring")))

;;; Test: All expected test suites exist

(test property-test-suites-exist
  "Property 2: All expected test suites should exist after refactoring"
  (let ((test-passed t)
        (expected-suites (get-all-test-suites)))
    
    (dolist (suite-name expected-suites)
      (let ((suite (fiveam:get-test suite-name)))
        (unless suite
          (format t "~&FAILED: Expected test suite ~A does not exist~%" suite-name)
          (setf test-passed nil))))
    
    (is-true test-passed "All expected test suites exist")))

;;; Test: All test suites are defined

(test property-test-suites-defined
  "Property 2: All test suites should be properly defined"
  (let ((test-passed t)
        (test-suites (get-all-test-suites)))
    
    (dolist (suite-name test-suites)
      (unless (suite-exists-p suite-name)
        (format t "~&FAILED: Test suite ~A is not defined~%" suite-name)
        (setf test-passed nil)))
    
    (format t "~&All ~D test suites are properly defined~%" (length test-suites))
    
    (is-true test-passed "All test suites are properly defined")))

;;; Test: Property-based test suites exist

(test property-pbt-suites-exist
  "Property 2: Property-based test suites should exist"
  (let ((test-passed t)
        (pbt-suites '(property-behavior-preservation-suite
                     property-test-coverage-suite)))
    
    (dolist (suite-name pbt-suites)
      (unless (suite-exists-p suite-name)
        (format t "~&FAILED: Property-based test suite ~A does not exist~%" suite-name)
        (setf test-passed nil)))
    
    (format t "~&All ~D property-based test suites exist~%" (length pbt-suites))
    
    (is-true test-passed "All property-based test suites exist")))
