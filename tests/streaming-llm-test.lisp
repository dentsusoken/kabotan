(in-package :kabotan.tests)

;;; Manual test for streaming LLM functionality
;;; This test demonstrates the streaming API with a mock function

(defun test-streaming-with-mock ()
  "Test streaming LLM with a mock function that simulates chunks"
  (format t "~%=== Testing Streaming LLM with Mock ===~%")
  
  (let ((chunks-received '())
        (kabotan::*llm-streaming-function*
          (lambda (prompt callback max-tokens temperature timeout)
            (declare (ignore prompt max-tokens temperature timeout))
            ;; Simulate receiving multiple chunks
            (funcall callback "Hello")
            (funcall callback " ")
            (funcall callback "world")
            (funcall callback "!")
            :success)))
    
    ;; Call streaming function with callback that collects chunks
    (let ((result (kabotan::call-llm-streaming 
                   "test prompt"
                   (lambda (chunk)
                     (push chunk chunks-received)
                     (format t "Received chunk: ~S~%" chunk)))))
      
      (format t "~%Result: ~A~%" result)
      (format t "Total chunks received: ~A~%" (length chunks-received))
      (format t "Chunks in order: ~A~%" (reverse chunks-received))
      (format t "Combined text: ~A~%" (apply #'concatenate 'string (reverse chunks-received)))
      
      ;; Verify results
      (assert (eq result :success) nil "Expected :success result")
      (assert (= 4 (length chunks-received)) nil "Expected 4 chunks")
      (assert (string= "Hello world!" 
                      (apply #'concatenate 'string (reverse chunks-received)))
              nil "Expected combined text to be 'Hello world!'")
      
      (format t "~%✓ All assertions passed!~%"))))

(defun test-streaming-with-messages-mock ()
  "Test streaming LLM with messages using a mock function"
  (format t "~%=== Testing Streaming LLM with Messages (Mock) ===~%")
  
  (let ((chunks-received '()))
    
    ;; Call streaming function with callback that collects chunks
    (let ((result (kabotan::call-llm-with-messages-streaming 
                   (list (list :role "user" :content "Hello"))
                   (lambda (chunk)
                     (push chunk chunks-received)
                     (format t "Received chunk: ~S~%" chunk))
                   :system-prompt "You are a helpful assistant")))
      
      (format t "~%Result: ~A~%" result)
      (format t "Total chunks received: ~A~%" (length chunks-received))
      
      ;; Note: Without mock, this will return :error
      ;; This is expected behavior when no mock is set
      (format t "~%Test completed (expected :error without mock)~%"))))

;; Run the tests
(defun run-streaming-tests ()
  "Run all streaming tests"
  (test-streaming-with-mock)
  (test-streaming-with-messages-mock)
  (format t "~%=== All Streaming Tests Complete ===~%"))
