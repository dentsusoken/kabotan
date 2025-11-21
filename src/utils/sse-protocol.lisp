(in-package :kabotan)

;;; ============================================================================
;;; Server-Sent Events (SSE) Protocol Utilities
;;; ============================================================================
;;;
;;; This module provides low-level utilities for handling Server-Sent Events
;;; (SSE) protocol used for streaming LLM responses from OpenAI-compatible APIs.
;;;
;;; SSE Protocol Overview:
;;; ---------------------
;;; SSE is a standard for server-to-client streaming over HTTP. The server sends
;;; events as text/event-stream with the following format:
;;;
;;;   data: <JSON payload>
;;;   
;;;   data: <JSON payload>
;;;   
;;;   data: [DONE]
;;;
;;; Each event starts with "data: " followed by the payload and ends with two
;;; newlines. The [DONE] marker signals stream completion.
;;;
;;; OpenAI Streaming Format:
;;; ------------------------
;;; OpenAI's streaming API sends JSON chunks with delta content:
;;;
;;;   data: {"choices":[{"delta":{"content":"Hello"}}]}
;;;   data: {"choices":[{"delta":{"content":" world"}}]}
;;;   data: [DONE]
;;;
;;; The content field contains incremental text that should be concatenated
;;; to build the complete response.
;;;
;;; ============================================================================

(defun parse-sse-chunk (line)
  "Parse a single SSE data line and extract content delta.
   
   This function implements the core SSE parsing logic for OpenAI-compatible
   streaming responses. It extracts the incremental content from each SSE event.
   
   SSE Line Format:
   ----------------
   Valid SSE lines start with 'data: ' followed by JSON payload:
     data: {\"choices\":[{\"delta\":{\"content\":\"text\"}}]}
   
   The [DONE] marker signals stream completion:
     data: [DONE]
   
   JSON Structure:
   ---------------
   The JSON payload follows OpenAI's streaming format:
   {
     \"choices\": [
       {
         \"delta\": {
           \"content\": \"incremental text\"
         }
       }
     ]
   }
   
   Only the 'content' field from the first choice's delta is extracted.
   
   Parameters:
   -----------
   - line: SSE data line string (e.g., 'data: {...}')
   
   Returns:
   --------
   - Content string if present (e.g., \"hello\")
   - nil if no content, [DONE] marker, or parsing error
   
   Error Handling:
   ---------------
   - Invalid JSON: Logs error and returns nil
   - Missing fields: Returns nil gracefully
   - Malformed SSE: Returns nil without error
   
   Examples:
   ---------
   (parse-sse-chunk \"data: {\\\"choices\\\":[{\\\"delta\\\":{\\\"content\\\":\\\"hi\\\"}}]}\")
   => \"hi\"
   
   (parse-sse-chunk \"data: [DONE]\")
   => nil"
  (when (and line (stringp line))
    ;; Step 1: Check if line starts with "data: " prefix
    ;; SSE protocol requires this prefix for data events
    (when (and (>= (length line) 6)
               (string= (subseq line 0 6) "data: "))
      ;; Step 2: Extract the data portion after "data: " prefix
      ;; Trim whitespace to handle various formatting styles
      (let ((data-part (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (subseq line 6))))
        ;; Step 3: Check for [DONE] marker
        ;; OpenAI sends this to signal stream completion
        (when (not (string= data-part "[DONE]"))
          ;; Step 4: Parse JSON and extract content
          ;; Use handler-case to gracefully handle malformed JSON
          (handler-case
              (let* (;; Parse JSON string to alist structure
                     (json-data (cl-json:decode-json-from-string data-part))
                     ;; Navigate JSON structure: choices array
                     (choices (cdr (assoc :choices json-data)))
                     ;; Get first choice (OpenAI typically sends one choice)
                     (first-choice (when choices (car choices)))
                     ;; Extract delta object (contains incremental changes)
                     (delta (when first-choice (cdr (assoc :delta first-choice))))
                     ;; Extract content field (the actual text chunk)
                     (content (when delta (cdr (assoc :content delta)))))
                ;; Return content string or nil if not present
                content)
            (error (e)
              ;; Log parsing error but don't propagate exception
              ;; This allows streaming to continue even if one chunk is malformed
              (log-error "SSE chunk parsing error" (format nil "~A" e))
              nil)))))))

(defun format-sse-data (data)
  "Format data as SSE data event following the SSE protocol specification.
   
   This function creates properly formatted SSE events that can be sent to
   clients via text/event-stream responses. The SSE protocol requires:
   1. Lines starting with 'data: '
   2. Double newline (\\n\\n) to mark event boundaries
   
   SSE Protocol Format:
   --------------------
   data: <payload>
   
   
   The double newline signals the end of an event. Clients (EventSource API)
   will fire an event when they receive the complete event.
   
   Parameters:
   -----------
   - data: String or data structure to send
           - If string: Used directly as payload
           - If other: JSON-encoded automatically
   
   Returns:
   --------
   - Formatted SSE string with 'data: ' prefix and double newline
   
   Examples:
   ---------
   (format-sse-data \"hello\")
   => \"data: hello\\n\\n\"
   
   (format-sse-data '((:chunk . \"hello\") (:type . \"content\")))
   => \"data: {\\\"chunk\\\":\\\"hello\\\",\\\"type\\\":\\\"content\\\"}\\n\\n\"
   
   Usage in Streaming:
   -------------------
   (funcall writer (format-sse-data '{\"chunk\": \"hello\"}'))
   
   This sends a single SSE event to the client, which will be received by
   the EventSource API's onmessage handler."
  (let ((data-str (if (stringp data)
                      data
                      ;; Auto-encode non-string data as JSON
                      (cl-json:encode-json-to-string data))))
    ;; Format as SSE event: "data: <payload>\n\n"
    (format nil "data: ~A~%~%" data-str)))

(defun format-sse-event (event-name data)
  "Format data as SSE event with custom event name for HTMX SSE extension.
   
   This function creates SSE events with custom event names, which is required
   for HTMX SSE extension to properly route events. The SSE protocol supports
   named events using the 'event:' field.
   
   SSE Named Event Format:
   -----------------------
   event: <event-name>
   data: <payload>
   
   
   The event name allows clients to listen for specific event types.
   HTMX SSE extension uses this to route events to different handlers.
   
   HTMX SSE Extension Usage:
   --------------------------
   HTMX expects these event names:
   - 'message': Content chunks to be swapped into the target
   - 'done': Completion signal (optional, can trigger cleanup)
   - 'error': Error messages to be displayed
   
   Parameters:
   -----------
   - event-name: Name of the SSE event (e.g., 'message', 'done', 'error')
   - data: String or data structure to send
           - If string: Used directly as payload
           - If other: JSON-encoded automatically
   
   Returns:
   --------
   - Formatted SSE string with event name, data, and double newline
   
   Examples:
   ---------
   (format-sse-event \"message\" \"<p>Hello</p>\")
   => \"event: message\\ndata: <p>Hello</p>\\n\\n\"
   
   (format-sse-event \"done\" '((:complete . t)))
   => \"event: done\\ndata: {\\\"complete\\\":true}\\n\\n\"
   
   (format-sse-event \"error\" \"Connection failed\")
   => \"event: error\\ndata: Connection failed\\n\\n\"
   
   Usage with HTMX:
   ----------------
   HTML:
   <div hx-ext=\"sse\" 
        sse-connect=\"/api/stream\"
        sse-swap=\"message\"
        hx-swap=\"beforeend\">
   </div>
   
   Server:
   (funcall writer (format-sse-event \"message\" \"<p>Chunk 1</p>\"))
   (funcall writer (format-sse-event \"message\" \"<p>Chunk 2</p>\"))
   (funcall writer (format-sse-event \"done\" \"\"))"
  (let ((data-str (if (stringp data)
                      data
                      ;; Auto-encode non-string data as JSON
                      (cl-json:encode-json-to-string data))))
    ;; Handle multi-line data by prefixing each line with "data: "
    ;; This is required by the SSE protocol
    (if (find #\Newline data-str)
        (with-output-to-string (out)
          (format out "event: ~A~%" event-name)
          (loop for line in (cl-ppcre:split "\\n" data-str)
                do (format out "data: ~A~%" line))
          (format out "~%"))
        ;; Single line data - simple format
        (format nil "event: ~A~%data: ~A~%~%" event-name data-str))))

(defun format-sse-error (error-message)
  "Format error as SSE error event for client-side error handling.
   
   This function creates a standardized error event that clients can detect
   and handle appropriately. The error event includes both the error message
   and a type field to distinguish it from content events.
   
   Error Event Structure:
   ----------------------
   {
     \"error\": \"error description\",
     \"type\": \"error\"
   }
   
   The 'type' field allows clients to differentiate between:
   - Content events: {\"chunk\": \"text\", \"type\": \"content\"}
   - Error events: {\"error\": \"message\", \"type\": \"error\"}
   - Done events: {\"done\": true, \"type\": \"done\"}
   
   Parameters:
   -----------
   - error-message: Error description string (e.g., \"Connection timeout\")
   
   Returns:
   --------
   - Formatted SSE error event string
   
   Example:
   --------
   (format-sse-error \"LLM API timeout\")
   => \"data: {\\\"error\\\":\\\"LLM API timeout\\\",\\\"type\\\":\\\"error\\\"}\\n\\n\"
   
   Client-Side Handling:
   ---------------------
   eventSource.onmessage = (event) => {
     const data = JSON.parse(event.data);
     if (data.type === 'error') {
       console.error('Streaming error:', data.error);
       // Show error UI, offer retry, etc.
     }
   };"
  (format-sse-data (list (cons :error error-message)
                         (cons :type "error"))))

(defun format-sse-html-error (error-message language)
  "Format error as HTML fragment in SSE error event for HTMX SSE extension.
   
   This function creates an HTML error alert that can be displayed directly
   by HTMX SSE extension. The error is sent as a named 'error' event with
   HTML content that HTMX can swap into the page.
   
   Parameters:
   -----------
   - error-message: Error description string
   - language: Language code for localized error messages
   
   Returns:
   --------
   - Formatted SSE error event with HTML content
   
   Example:
   --------
   (format-sse-html-error \"Connection failed\" \"en\")
   => \"event: error\\ndata: <div class=\\\"alert alert-error\\\">...</div>\\n\\n\"
   
   HTMX Usage:
   -----------
   The error event can be caught by HTMX error handlers or displayed
   in a designated error container."
  (let ((error-html (generate-alert 
                     :type :error
                     :message error-message
                     :class "mb-4")))
    (format-sse-event "error" error-html)))

(defun create-sse-response (generator-fn)
  "Create a true streaming SSE response using Clack's streaming protocol.
   
   This function implements Clack's streaming response protocol to enable
   real-time Server-Sent Events. It provides a clean abstraction over the
   low-level streaming mechanics.
   
   Clack Streaming Protocol:
   -------------------------
   Clack supports streaming by returning a function instead of a response list.
   This function receives a 'responder' callback:
   
   1. Call responder with (status headers) - NO body
   2. Responder returns a 'writer' function
   3. Call writer with data chunks: (writer octets :close nil)
   4. Close stream: (writer empty-octets :close t)
   
   The absence of a body in step 1 signals streaming mode to Clack.
   
   Writer Function Signature:
   --------------------------
   (writer body &key start end close)
   
   - body: nil, string, or (vector (unsigned-byte 8))
   - close: t to close stream, nil to keep open
   
   This implementation converts strings to UTF-8 octets automatically.
   
   Callback Pattern:
   -----------------
   The generator-fn receives a simplified writer that accepts strings:
   
   (lambda (writer)
     (funcall writer (format-sse-data '{\"chunk\": \"hello\"}'))
     (funcall writer (format-sse-data '{\"chunk\": \"world\"}'))
     (funcall writer (format-sse-data '{\"done\": true}')))
   
   The writer handles UTF-8 encoding and keeps the connection open until
   all chunks are sent.
   
   Error Handling:
   ---------------
   Enhanced error handling at multiple levels:
   
   1. Generator errors: Caught and logged
   2. Error events: Sent to client before closing
   3. Broken pipe: Detected and handled gracefully (client disconnect)
   4. Stream closure: Always attempted, even on error
   
   Broken Pipe Handling:
   ---------------------
   When clients disconnect (close browser, navigate away), the server
   receives a \"Broken pipe\" error. This is normal and expected, so it's
   logged at DEBUG level rather than ERROR level.
   
   Parameters:
   -----------
   - generator-fn: Function that accepts a writer callback
                   The writer takes a string and sends it to the client
                   The generator should call writer for each SSE event
   
   Returns:
   --------
   - Function that accepts a responder callback (Clack streaming protocol)
   
   Example Usage:
   --------------
   (create-sse-response
     (lambda (writer)
       ;; Send content chunks
       (funcall writer (format-sse-data '{\"chunk\": \"Hello\"}'))
       (funcall writer (format-sse-data '{\"chunk\": \" world\"}'))
       ;; Send completion marker
       (funcall writer (format-sse-data '{\"done\": true}'))))
   
   HTTP Response Headers:
   ----------------------
   The response includes these headers for SSE:
   - Content-Type: text/event-stream
   - Cache-Control: no-cache
   - Connection: keep-alive
   - X-Accel-Buffering: no (disables nginx buffering)"
  ;; Return a function that follows Clack's streaming protocol
  (lambda (responder)
    ;; Call responder with status and headers, but NO body
    ;; This signals to Clack that we want streaming mode
    (let ((writer (funcall responder
                          (list 200
                                '(:content-type "text/event-stream"
                                  :cache-control "no-cache"
                                  :connection "keep-alive"
                                  :x-accel-buffering "no"))))
          (stream-closed nil))
      ;; writer is now a function that writes directly to the HTTP stream
      ;; It expects: (writer body &key start end close)
      ;; where body can be: nil, string, or (vector (unsigned-byte 8))
      
      ;; Wrap in handler-case to catch any errors
      (handler-case
          (progn
            ;; Create a wrapper that converts strings to octets
            (let ((stream-writer (lambda (data)
                                  ;; Convert string to octets
                                  (let ((octets (flexi-streams:string-to-octets 
                                                data 
                                                :external-format :utf-8)))
                                    ;; Write to stream with :close nil to keep connection open
                                    (funcall writer octets :close nil)))))
              ;; Call the generator function with our stream writer
              (funcall generator-fn stream-writer))
            ;; Close the stream after all chunks are sent
            ;; Pass empty vector instead of nil to avoid length calculation error
            (funcall writer (make-array 0 :element-type '(unsigned-byte 8)) :close t)
            (setf stream-closed t))
        (error (condition)
          ;; Check if this is a "Broken pipe" error (client disconnected)
          ;; This is a normal occurrence when users stop streaming or navigate away
          (let ((error-msg (format nil "~A" condition)))
            (if (search "Broken pipe" error-msg)
                ;; Client disconnected - this is normal, just log at debug level
                (format t "[DEBUG] Client disconnected during streaming (broken pipe)~%")
                ;; Other errors - log as errors
                (log-error "SSE streaming error" error-msg)))
          
          ;; Try to send error event to client before closing (only if not broken pipe)
          (unless stream-closed
            (handler-case
                (let* ((error-msg (format nil "Streaming error: ~A" condition)))
                  ;; Only try to send error event if it's not a broken pipe
                  (unless (search "Broken pipe" error-msg)
                    (let* ((error-event (format-sse-error error-msg))
                           (octets (flexi-streams:string-to-octets 
                                   error-event
                                   :external-format :utf-8)))
                      ;; Send error event
                      (funcall writer octets :close nil))))
              (error (send-error)
                ;; If we can't send error event, check if it's broken pipe
                (let ((send-error-msg (format nil "~A" send-error)))
                  (unless (search "Broken pipe" send-error-msg)
                    (log-error "Failed to send SSE error event" send-error-msg))))))
          
          ;; Ensure stream is closed gracefully
          (unless stream-closed
            (handler-case
                (progn
                  (funcall writer (make-array 0 :element-type '(unsigned-byte 8)) :close t)
                  (setf stream-closed t))
              (error (close-error)
                ;; Log if we can't close the stream (unless it's broken pipe)
                (let ((close-error-msg (format nil "~A" close-error)))
                  (unless (search "Broken pipe" close-error-msg)
                    (log-error "Failed to close SSE stream" close-error-msg)))))))))))
