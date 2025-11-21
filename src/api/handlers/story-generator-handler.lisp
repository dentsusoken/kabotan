(in-package :kabotan)

;;; Story Generator API Handler

;;; ----------------------------------------------------------------------------
;;; Parameter Extraction
;;; ----------------------------------------------------------------------------

(defun extract-story-generator-params (params)
  "Extract and validate parameters for story generator request.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - param-plist: Property list with validated parameters, or NIL on error
   - error-message: Error message string if validation fails, or NIL on success
   
   The function extracts and validates:
   - language: Validated language code (defaults to 'en')
   - name: Required, non-empty string
   - theme: Required, non-empty string
   - style: Optional, validated story style (defaults to 'classic')"
  (let ((language (extract-and-validate-language params)))
    (multiple-value-bind (valid-p missing-keys)
        (validate-required-params params '("name" "theme"))
      (if valid-p
          (let* ((style (get-param params "style" "classic"))
                 (validated-style (if (validate-story-style style)
                                     style
                                     "classic")))
            (values (list :language language
                         :name (sanitize-input (get-param params "name"))
                         :theme (sanitize-input (get-param params "theme"))
                         :style validated-style)
                    nil))
          (values nil "Name and theme are required")))))

;;; ----------------------------------------------------------------------------
;;; Business Logic
;;; ----------------------------------------------------------------------------

(defun process-story-generator (params)
  "Process story generator request with validated parameters.
   
   Parameters:
   -----------
   - params: Property list with validated parameters
     - :language - Language code
     - :name - Character name
     - :theme - Story theme
     - :style - Story style
   
   Returns:
   --------
   - result: LLM response string, or NIL on error
   
   This function builds the prompt and calls the LLM service."
  (let* ((language (getf params :language))
         (name (getf params :name))
         (theme (getf params :theme))
         (style (getf params :style))
         (prompt (build-story-generator-prompt name style theme language)))
    (call-llm-with-retry prompt :temperature 0.9)))

;;; ----------------------------------------------------------------------------
;;; Public Handlers
;;; ----------------------------------------------------------------------------

(defun handle-story-generator-request (params)
  "Handles story generator API requests.
   
   Parameters:
   - params: Alist of request parameters from Ningle
   
   Returns:
   - HTTP response list: (status-code headers body)"
  (handler-case
      (multiple-value-bind (extracted-params error)
          (extract-story-generator-params params)
        (if error
            (build-validation-error-response error 
                                            (extract-and-validate-language params))
            (let ((result (process-story-generator extracted-params)))
              (if result
                  `(200 (:content-type "text/html; charset=utf-8")
                    (,(format-story-response 
                        result 
                        (getf extracted-params :language))))
                  (build-service-error-response 
                    "LLM service unavailable"
                    (getf extracted-params :language))))))
    (error (condition)
      (log-error "story-generator endpoint" condition)
      (build-service-error-response 
        (format nil "~A" condition)
        (get-param params "language" "en")))))

(defun handle-story-generator-streaming (params)
  "Handles streaming story generator API requests with HTMX SSE extension.
   
   Parameters:
   - params: Alist of request parameters from Ningle
   
   Returns:
   - Clack streaming response with SSE format
   
   SSE Event Format (HTMX):
   ------------------------
   Content: event: message\\ndata: <span>text</span>\\n\\n
   Done: event: done\\ndata: \\n\\n
   Error: event: error\\ndata: <div class=\"alert alert-error\">...</div>\\n\\n"
  (handler-case
      (multiple-value-bind (extracted-params error)
          (extract-story-generator-params params)
        (if error
            (build-validation-error-response error 
                                            (extract-and-validate-language params)
                                            :streaming-p t)
            (let* ((language (getf extracted-params :language))
                   (name (getf extracted-params :name))
                   (theme (getf extracted-params :theme))
                   (style (getf extracted-params :style)))
              (create-htmx-streaming-handler
               (lambda () (build-story-generator-prompt name style theme language))
               (lambda (chunk)
                 ;; Wrap each chunk in a span for streaming display
                 (format nil "<span>~A</span>" (escape-html chunk)))
               :temperature 0.9
               :language language))))
    (error (condition)
      (log-error "story-generator-streaming endpoint" condition)
      `(500 (:content-type "application/json")
        (,(cl-json:encode-json-to-string
            (list (cons :error (format nil "~A" condition))
                  (cons :type "error"))))))))
