#!/usr/bin/env ros
;;; Manual test script for streaming API communication
;;; This script tests actual SSE streaming with OpenAI-compatible API

(ql:quickload '(:dexador :cl-json :kabotan) :silent t)

(in-package :kabotan)

(defun test-streaming-api ()
  "Test actual streaming communication with OpenAI-compatible API"
  (let* ((api-key (or (uiop:getenv "OPENAI_API_KEY")
                      (uiop:getenv "OPENAI_KEY")
                      "dummy"))
         (model (or (uiop:getenv "OPENAI_MODEL") "gemma3-12b"))
         (host (or (uiop:getenv "OPENAI_HOST")
                   "http://localhost:8080/v1/chat/completions"))
         (request-body (cl-json:encode-json-to-string
                        `((:model . ,model)
                          (:messages . #(((:role . "user")
                                         (:content . "Write a detailed 300-word Halloween story about a vampire and a witch who become friends. Include vivid descriptions and dialogue."))))
                          (:stream . t)
                          (:max--tokens . 500)
                          (:temperature . 0.7))))
         (chunk-count 0)
         (total-content "")
         (start-time (get-internal-real-time)))
    
    (format t "~%=== Streaming API Test ===~%")
    (format t "Host: ~A~%" host)
    (format t "Model: ~A~%" model)
    (format t "~%Starting streaming request...~%~%")
    
    (handler-case
        (let ((stream (dex:request host
                                   :method :post
                                   :headers `(("Content-Type" . "application/json")
                                             ("Authorization" . ,(format nil "Bearer ~A" api-key)))
                                   :content request-body
                                   :want-stream t
                                   :read-timeout 30)))
          (unwind-protect
               (loop for line = (read-line stream nil nil)
                     while line
                     do (progn
                          ;; Try to parse with our SSE utility
                          (let ((content (parse-sse-chunk line)))
                            (when content
                              (incf chunk-count)
                              (setf total-content (concatenate 'string total-content content))
                              ;; Show progress every 10 chunks
                              (when (zerop (mod chunk-count 10))
                                (format t "[PROGRESS] ~D chunks received, ~D chars so far...~%" 
                                        chunk-count (length total-content)))
                              ;; Show individual chunks for first 5 and last few
                              (when (or (<= chunk-count 5) (search "[DONE]" line))
                                (format t "[CHUNK ~D] ~S~%" chunk-count content))))))
            (close stream)))
      (error (e)
        (format t "~%[ERROR] Streaming failed: ~A~%" e)
        (return-from test-streaming-api nil)))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed-seconds (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "~%=== Test Results ===~%")
      (format t "Total chunks received: ~D~%" chunk-count)
      (format t "Total characters: ~D~%" (length total-content))
      (format t "Elapsed time: ~,2F seconds~%" elapsed-seconds)
      (format t "Average chunks/sec: ~,2F~%" (/ chunk-count elapsed-seconds))
      (format t "~%Complete response:~%~A~%" total-content)
      (format t "~%Test ~A~%" (if (> chunk-count 0) "PASSED ✓" "FAILED ✗"))
      
      (> chunk-count 0))))

;; Run the test
(let ((result (test-streaming-api)))
  (uiop:quit (if result 0 1)))
