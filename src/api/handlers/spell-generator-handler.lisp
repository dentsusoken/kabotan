(in-package :kabotan)

;;; ============================================================================
;;; Spell Generator API Handler
;;; ============================================================================
;;;
;;; This handler generates magical spells or phrases on demand using LLM.
;;; It supports both non-streaming (HTML) and streaming (SSE) responses.
;;;
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Parameter Extraction
;;; ----------------------------------------------------------------------------

(defun extract-spell-generator-params (params)
  "Extract and validate parameters for spell generator request.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - param-plist: Property list with validated parameters
     Format: (:language \"en\")
   - error-message: Error message string if validation fails, NIL otherwise
   
   Validation:
   -----------
   - Language parameter is optional, defaults to 'en'
   - Language is validated and defaults to 'en' if invalid"
  (let ((language (extract-and-validate-language params)))
    ;; Return validated parameters (no validation errors possible)
    (values (list :language language) nil)))

;;; ----------------------------------------------------------------------------
;;; Non-Streaming Handler
;;; ----------------------------------------------------------------------------

(defun handle-spell-generator-request (params)
  "Handles spell generator API requests.
   
   This handler generates a new magical spell or phrase using the LLM service.
   Each request generates a unique spell with high temperature (0.9) for creativity.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)
   
   Response Format:
   ----------------
   Success (200): HTML formatted spell with explanation
   Error (500): HTML formatted error message"
  (wrap-handler
   (lambda ()
     (multiple-value-bind (extracted-params error)
         (extract-spell-generator-params params)
       (declare (ignore error))  ; No validation errors possible
       (let* ((language (getf extracted-params :language))
              (prompt (build-spell-generator-prompt language))
              (result (call-llm-with-retry prompt :temperature 0.9)))
         (if result
             `(200 (:content-type "text/html; charset=utf-8")
               (,(format-spell-generator-response result language)))
             (build-service-error-response 
              "LLM service unavailable" 
              language
              :streaming-p nil)))))
   params
   :streaming-p nil))

;;; ----------------------------------------------------------------------------
;;; Streaming Handler
;;; ----------------------------------------------------------------------------

(defun handle-spell-generator-streaming (params)
  "Handles streaming spell generator API requests with HTMX SSE extension.
   
   This handler generates a new magical spell or phrase using streaming SSE
   with HTML fragments for HTMX SSE extension. Each request generates a unique
   spell with high temperature (0.9) for creativity.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - Clack streaming response with SSE format
   
   SSE Event Format (HTMX):
   ------------------------
   Start: event: message\\ndata: <div class=\"text-center\">...\\n\\n
   Content: event: message\\ndata: <span>text</span>\\n\\n
   Done: event: done\\ndata: \\n\\n
   Error: event: error\\ndata: <div class=\"alert alert-error\">...</div>\\n\\n"
  (wrap-handler
   (lambda ()
     (multiple-value-bind (extracted-params error)
         (extract-spell-generator-params params)
       (declare (ignore error))  ; No validation errors possible
       (let ((language (getf extracted-params :language)))
         (create-sse-response
          (lambda (writer)
            ;; Send start-response event to trigger container creation in JavaScript
            (funcall writer 
                    (format-sse-event "start-response"
                     (cl-json:encode-json-to-string 
                      (list (cons :feature "spell-generator")))))
            
            ;; Stream the content chunks
            (let ((result (call-llm-streaming 
                           (build-spell-generator-prompt language)
                           (lambda (chunk)
                             (funcall writer 
                                     (format-sse-event "message"
                                      (format nil "<span>~A</span>" (escape-html chunk)))))
                           :temperature 0.9)))
              
              (if (eq result :error)
                  ;; Send HTML error alert as 'error' event
                  (funcall writer (format-sse-html-error "LLM service error" language))
                  ;; Send empty 'done' event
                  (funcall writer (format-sse-event "done" "")))))))))
   params
   :streaming-p t))
