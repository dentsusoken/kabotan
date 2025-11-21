(in-package :kabotan.tests)

;;; Property-Based Testing Utilities
;;; This file provides utilities for property-based testing using cl-quickcheck

;;; Test configuration
(defparameter *property-test-iterations* 100
  "Number of iterations to run for each property test")

;;; Utility functions for property testing

(defun run-property-test (name property-fn &key (iterations *property-test-iterations*))
  "Run a property test with the specified number of iterations.
   NAME is a string describing the property.
   PROPERTY-FN is a function that returns T if the property holds, NIL otherwise.
   Returns T if all iterations pass, NIL otherwise."
  (format t "~&Running property test: ~A (~D iterations)~%" name iterations)
  (dotimes (i iterations)
    (unless (funcall property-fn)
      (format t "~&Property test FAILED at iteration ~D~%" (1+ i))
      (return-from run-property-test nil)))
  (format t "~&Property test PASSED: All ~D iterations succeeded~%" iterations)
  t)

;;; Generator utilities for creating random test data

(defun random-string (&optional (max-length 100))
  "Generate a random string with length up to MAX-LENGTH"
  (let ((length (random (1+ max-length))))
    (coerce (loop repeat length
                  collect (code-char (+ 32 (random 95))))
            'string)))

(defun random-list (generator &optional (max-length 20))
  "Generate a random list using GENERATOR function"
  (let ((length (random (1+ max-length))))
    (loop repeat length collect (funcall generator))))

(defun random-choice (&rest choices)
  "Randomly select one item from CHOICES"
  (nth (random (length choices)) choices))

(defun random-boolean ()
  "Generate a random boolean value"
  (zerop (random 2)))

(defun random-integer (&optional (min 0) (max 1000))
  "Generate a random integer between MIN and MAX"
  (+ min (random (- max min))))

;;; Property test result tracking

(defvar *property-test-results* nil
  "List of property test results for reporting")

(defun record-property-test-result (name passed iterations)
  "Record the result of a property test"
  (push (list :name name :passed passed :iterations iterations)
        *property-test-results*))

(defun clear-property-test-results ()
  "Clear all recorded property test results"
  (setf *property-test-results* nil))

(defun report-property-test-results ()
  "Report all property test results"
  (format t "~&~%=== Property Test Results ===~%")
  (dolist (result (reverse *property-test-results*))
    (format t "~&~A: ~A (~D iterations)~%"
            (getf result :name)
            (if (getf result :passed) "PASSED" "FAILED")
            (getf result :iterations)))
  (format t "~&~%Total: ~D tests, ~D passed, ~D failed~%"
          (length *property-test-results*)
          (count-if (lambda (r) (getf r :passed)) *property-test-results*)
          (count-if (lambda (r) (not (getf r :passed))) *property-test-results*)))
