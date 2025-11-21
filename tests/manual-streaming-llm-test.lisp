#!/usr/bin/env ros
;;; Manual test script for streaming LLM service functions
;;; This script tests the new call-openai-api-streaming and call-llm-streaming functions

(ql:quickload '(:kabotan) :silent t)

(in-package :kabotan)

(defun test-call-openai-api-streaming ()
  "Test call-openai-api-streaming function with actual API"
  (format t "~%=== Testing call-openai-api-streaming ===~%")
  
  (let ((chunk-count 0)
        (total-content "")
        (start-time (get-internal-real-time)))
    
    (format t "Starting streaming request...~%~%")
    
    (let ((result (call-openai-api-streaming
                   "Write a short 100-word Halloween story about a friendly ghost."
                   (lambda (chunk)
                     (incf chunk-count)
                     (setf total-content (concatenate 'string total-content chunk))
                     ;; Show progress every 10 chunks
                     (when (zerop (mod chunk-count 10))
                       (format t "[PROGRESS] ~D chunks received, ~D chars so far...~%" 
                               chunk-count (length total-content)))
                     ;; Show first 5 chunks
                     (when (<= chunk-count 5)
                       (format t "[CHUNK ~D] ~S~%" chunk-count chunk)))
                   :max-tokens 200
                   :temperature 0.7
                   :timeout 30)))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed-seconds (/ (- end-time start-time) internal-time-units-per-second)))
        (format t "~%=== Test Results ===~%")
        (format t "Result: ~A~%" result)
        (format t "Total chunks received: ~D~%" chunk-count)
        (format t "Total characters: ~D~%" (length total-content))
        (format t "Elapsed time: ~,2F seconds~%" elapsed-seconds)
        (when (> chunk-count 0)
          (format t "Average chunks/sec: ~,2F~%" (/ chunk-count elapsed-seconds)))
        (format t "~%Complete response:~%~A~%" total-content)
        (format t "~%Test ~A~%" (if (eq result :success) "PASSED ✓" "FAILED ✗"))
        
        (eq result :success)))))

(defun test-call-llm-streaming ()
  "Test call-llm-streaming wrapper function with actual API"
  (format t "~%=== Testing call-llm-streaming ===~%")
  
  (let ((chunk-count 0)
        (total-content "")
        (start-time (get-internal-real-time)))
    
    (format t "Starting streaming request...~%~%")
    
    (let ((result (call-llm-streaming
                   "Tell me a very short Halloween joke in one sentence."
                   (lambda (chunk)
                     (incf chunk-count)
                     (setf total-content (concatenate 'string total-content chunk))
                     (format t "[CHUNK ~D] ~S~%" chunk-count chunk))
                   :max-tokens 100
                   :temperature 0.8
                   :timeout 30)))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed-seconds (/ (- end-time start-time) internal-time-units-per-second)))
        (format t "~%=== Test Results ===~%")
        (format t "Result: ~A~%" result)
        (format t "Total chunks received: ~D~%" chunk-count)
        (format t "Total characters: ~D~%" (length total-content))
        (format t "Elapsed time: ~,2F seconds~%" elapsed-seconds)
        (format t "~%Complete response:~%~A~%" total-content)
        (format t "~%Test ~A~%" (if (eq result :success) "PASSED ✓" "FAILED ✗"))
        
        (eq result :success)))))

(defun test-call-llm-with-messages-streaming ()
  "Test call-llm-with-messages-streaming function with actual API"
  (format t "~%=== Testing call-llm-with-messages-streaming ===~%")
  
  (let ((chunk-count 0)
        (total-content "")
        (start-time (get-internal-real-time))
        (messages (list (list :role "user" :content "What is Halloween?")
                       (list :role "assistant" :content "Halloween is a holiday celebrated on October 31st.")
                       (list :role "user" :content "Tell me one interesting fact about it in one sentence."))))
    
    (format t "Starting streaming request with message history...~%~%")
    
    (let ((result (call-llm-with-messages-streaming
                   messages
                   (lambda (chunk)
                     (incf chunk-count)
                     (setf total-content (concatenate 'string total-content chunk))
                     (format t "[CHUNK ~D] ~S~%" chunk-count chunk))
                   :max-tokens 100
                   :temperature 0.7
                   :timeout 30
                   :system-prompt "You are a helpful Halloween expert.")))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed-seconds (/ (- end-time start-time) internal-time-units-per-second)))
        (format t "~%=== Test Results ===~%")
        (format t "Result: ~A~%" result)
        (format t "Total chunks received: ~D~%" chunk-count)
        (format t "Total characters: ~D~%" (length total-content))
        (format t "Elapsed time: ~,2F seconds~%" elapsed-seconds)
        (format t "~%Complete response:~%~A~%" total-content)
        (format t "~%Test ~A~%" (if (eq result :success) "PASSED ✓" "FAILED ✗"))
        
        (eq result :success)))))

;; Run all tests
(defun run-all-streaming-tests ()
  "Run all streaming LLM tests"
  (let ((test1 (test-call-openai-api-streaming))
        (test2 (test-call-llm-streaming))
        (test3 (test-call-llm-with-messages-streaming)))
    
    (format t "~%~%=== All Tests Summary ===~%")
    (format t "call-openai-api-streaming: ~A~%" (if test1 "PASSED ✓" "FAILED ✗"))
    (format t "call-llm-streaming: ~A~%" (if test2 "PASSED ✓" "FAILED ✗"))
    (format t "call-llm-with-messages-streaming: ~A~%" (if test3 "PASSED ✓" "FAILED ✗"))
    
    (and test1 test2 test3)))

;; Run the tests
(let ((result (run-all-streaming-tests)))
  (uiop:quit (if result 0 1)))
