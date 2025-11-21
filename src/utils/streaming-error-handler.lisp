(in-package :kabotan)

;;; Streaming Error Handler Utilities
;;; Provides enhanced error handling for streaming responses

(defun create-streaming-callback-with-tracking (writer)
  "Create a streaming callback that tracks chunks sent and handles errors.
   
   Parameters:
   - writer: SSE writer function
   
   Returns:
   - Plist with :callback function and :get-stats function
   
   Example:
   (let* ((tracked (create-streaming-callback-with-tracking writer))
          (callback (getf tracked :callback))
          (get-stats (getf tracked :get-stats)))
     ;; Use callback for streaming
     (call-llm-streaming prompt callback)
     ;; Get statistics
     (funcall get-stats)) ; => (:chunks 10 :chars 500)"
  (let ((chunks-sent 0)
        (total-chars 0))
    (list :callback
          (lambda (chunk)
            ;; Track statistics
            (incf chunks-sent)
            (incf total-chars (length chunk))
            ;; Send chunk as SSE data event
            (funcall writer 
                    (format-sse-data 
                     (list (cons :chunk chunk)
                           (cons :type "content")))))
          :get-stats
          (lambda ()
            (list :chunks chunks-sent :chars total-chars)))))

(defun handle-streaming-result (result writer context chunks-sent)
  "Handle the result of a streaming LLM call.
   
   Parameters:
   - result: Result from streaming call (:success or :error)
   - writer: SSE writer function
   - context: Context string for logging (e.g., 'character-chat')
   - chunks-sent: Number of chunks sent before completion/error
   
   Side effects:
   - Sends completion or error event to client
   - Logs errors with context"
  (if (eq result :error)
      (progn
        ;; Log error with partial response info
        (log-error (format nil "~A-streaming LLM error" context)
                  (format nil "Failed after sending ~A chunks" chunks-sent))
        ;; Send error event (partial content preserved on client)
        (funcall writer 
                (format-sse-error "LLM service error")))
      ;; Send completion event
      (funcall writer 
              (format-sse-data 
               (list (cons :done t)
                     (cons :type "done"))))))

(defun wrap-streaming-handler (writer streaming-fn context)
  "Wrap a streaming handler with error handling and tracking.
   
   Parameters:
   - writer: SSE writer function
   - streaming-fn: Function that performs streaming (accepts callback)
   - context: Context string for logging (e.g., 'character-chat')
   
   Side effects:
   - Calls streaming-fn with tracked callback
   - Handles errors and sends appropriate events
   - Logs errors with context and statistics"
  (let* ((tracked (create-streaming-callback-with-tracking writer))
         (callback (getf tracked :callback))
         (get-stats (getf tracked :get-stats)))
    (handler-case
        (let ((result (funcall streaming-fn callback)))
          ;; Get statistics
          (let* ((stats (funcall get-stats))
                 (chunks-sent (getf stats :chunks)))
            ;; Handle result
            (handle-streaming-result result writer context chunks-sent)))
      (error (condition)
        ;; Get statistics for error logging
        (let* ((stats (funcall get-stats))
               (chunks-sent (getf stats :chunks))
               (chars-sent (getf stats :chars)))
          ;; Log streaming error with context
          (log-error (format nil "~A-streaming handler error" context)
                    (format nil "~A (sent ~A chunks, ~A chars)" 
                            condition chunks-sent chars-sent))
          ;; Send error event to client
          (funcall writer 
                  (format-sse-error 
                   (format nil "Streaming error: ~A" condition))))))))

