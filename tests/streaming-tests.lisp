(in-package :kabotan.tests)

;;; Streaming SSE Utility Tests

(def-suite streaming-suite
  :in kabotan-suite
  :description "Streaming SSE utility tests")

(in-suite streaming-suite)

(test parse-sse-chunk-valid-content
  "Test that parse-sse-chunk extracts content from valid SSE data lines"
  (let ((line "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}"))
    (is (string= "hello" (parse-sse-chunk line))))
  (let ((line "data: {\"choices\":[{\"delta\":{\"content\":\"world\"}}]}"))
    (is (string= "world" (parse-sse-chunk line))))
  (let ((line "data: {\"choices\":[{\"delta\":{\"content\":\" test\"}}]}"))
    (is (string= " test" (parse-sse-chunk line)))))

(test parse-sse-chunk-done-marker
  "Test that parse-sse-chunk returns nil for [DONE] marker"
  (let ((line "data: [DONE]"))
    (is (null (parse-sse-chunk line)))))

(test parse-sse-chunk-empty-content
  "Test that parse-sse-chunk handles empty content"
  (let ((line "data: {\"choices\":[{\"delta\":{\"content\":\"\"}}]}"))
    (is (string= "" (parse-sse-chunk line)))))

(test parse-sse-chunk-no-content-field
  "Test that parse-sse-chunk returns nil when content field is missing"
  (let ((line "data: {\"choices\":[{\"delta\":{}}]}"))
    (is (null (parse-sse-chunk line))))
  (let ((line "data: {\"choices\":[{\"delta\":{\"role\":\"assistant\"}}]}"))
    (is (null (parse-sse-chunk line)))))

(test parse-sse-chunk-malformed-json
  "Test that parse-sse-chunk handles malformed JSON gracefully"
  (let ((line "data: {invalid json"))
    (is (null (parse-sse-chunk line))))
  (let ((line "data: not json at all"))
    (is (null (parse-sse-chunk line)))))

(test parse-sse-chunk-no-data-prefix
  "Test that parse-sse-chunk returns nil for lines without 'data: ' prefix"
  (let ((line "{\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}"))
    (is (null (parse-sse-chunk line))))
  (let ((line "event: message"))
    (is (null (parse-sse-chunk line)))))

(test parse-sse-chunk-nil-or-empty
  "Test that parse-sse-chunk handles nil or empty input"
  (is (null (parse-sse-chunk nil)))
  (is (null (parse-sse-chunk "")))
  (is (null (parse-sse-chunk "data: "))))

(test parse-sse-chunk-whitespace-handling
  "Test that parse-sse-chunk handles whitespace correctly"
  (let ((line "data:  {\"choices\":[{\"delta\":{\"content\":\"test\"}}]}  "))
    (is (string= "test" (parse-sse-chunk line)))))

(test format-sse-data-string
  "Test that format-sse-data produces correct SSE format for strings"
  (let ((result (format-sse-data "test message")))
    (is (stringp result))
    (is (search "data: test message" result))
    (is (search (format nil "~%~%") result))))

(test format-sse-data-json-object
  "Test that format-sse-data produces correct SSE format for JSON objects"
  (let ((result (format-sse-data (list (cons :chunk "hello")
                                       (cons :type "content")))))
    (is (stringp result))
    (is (search "data: " result))
    (is (search "\"chunk\"" result))
    (is (search "\"hello\"" result))
    (is (search "\"type\"" result))
    (is (search "\"content\"" result))
    (is (search (format nil "~%~%") result))))

(test format-sse-data-double-newline
  "Test that format-sse-data ends with double newline"
  (let ((result (format-sse-data "test")))
    (is (string= (format nil "~%~%") (subseq result (- (length result) 2))))))

(test format-sse-error-structure
  "Test that format-sse-error produces correct error event structure"
  (let ((result (format-sse-error "Test error message")))
    (is (stringp result))
    (is (search "data: " result))
    (is (search "\"error\"" result))
    (is (search "Test error message" result))
    (is (search "\"type\"" result))
    (is (search "\"error\"" result))))

(test format-sse-error-format
  "Test that format-sse-error produces valid SSE format"
  (let ((result (format-sse-error "Connection failed")))
    (is (search "data: " result))
    (is (search (format nil "~%~%") result))))

(test create-sse-response-structure
  "Test that create-sse-response produces correct Clack streaming response function"
  (let ((response (create-sse-response
                   (lambda (writer)
                     (funcall writer (format-sse-data "test"))))))
    ;; New implementation returns a function that accepts a responder callback
    (is (functionp response))))

(test create-sse-response-headers
  "Test that create-sse-response sets correct SSE headers via responder"
  (let* ((response-fn (create-sse-response
                       (lambda (writer)
                         (funcall writer (format-sse-data "test")))))
         (captured-status nil)
         (captured-headers nil)
         (mock-responder (lambda (response-list)
                          ;; Capture status and headers from responder call
                          (setf captured-status (first response-list))
                          (setf captured-headers (second response-list))
                          ;; Return a mock writer function
                          (lambda (data &key start end close)
                            (declare (ignore data start end close))
                            nil))))
    ;; Call the response function with mock responder
    (funcall response-fn mock-responder)
    ;; Verify captured values
    (is (= 200 captured-status))
    (is (string= "text/event-stream" (getf captured-headers :content-type)))
    (is (string= "no-cache" (getf captured-headers :cache-control)))
    (is (string= "keep-alive" (getf captured-headers :connection)))
    (is (string= "no" (getf captured-headers :x-accel-buffering)))))

(test create-sse-response-status-code
  "Test that create-sse-response returns 200 status code via responder"
  (let* ((response-fn (create-sse-response
                       (lambda (writer)
                         (funcall writer (format-sse-data "test")))))
         (captured-status nil)
         (mock-responder (lambda (response-list)
                          (setf captured-status (first response-list))
                          ;; Return a mock writer function
                          (lambda (data &key start end close)
                            (declare (ignore data start end close))
                            nil))))
    ;; Call the response function with mock responder
    (funcall response-fn mock-responder)
    ;; Verify status code
    (is (= 200 captured-status))))
