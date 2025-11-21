(in-package :kabotan)

;;; Monster Diagnostic API Handler

;;; ----------------------------------------------------------------------------
;;; Parameter Extraction
;;; ----------------------------------------------------------------------------

(defun extract-monster-diagnostic-params (params)
  "Extract and validate parameters for monster diagnostic request.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - param-plist: Property list with validated parameters, or NIL on error
   - error-message: Error message string if validation fails, or NIL on success
   
   The function extracts and validates:
   - language: Validated language code (defaults to 'en')
   - favorite-food: Required, non-empty string
   - sleep-schedule: Required, non-empty string
   - hobby: Required, non-empty string
   - fear: Required, non-empty string"
  (let ((language (extract-and-validate-language params)))
    (multiple-value-bind (valid-p missing-keys)
        (validate-required-params params 
                                 '("favorite_food" "sleep_schedule" "hobby" "fear"))
      (if valid-p
          (values (list :language language
                       :favorite-food (sanitize-input (get-param params "favorite_food"))
                       :sleep-schedule (sanitize-input (get-param params "sleep_schedule"))
                       :hobby (sanitize-input (get-param params "hobby"))
                       :fear (sanitize-input (get-param params "fear")))
                  nil)
          (values nil "All fields are required")))))

;;; ----------------------------------------------------------------------------
;;; Business Logic
;;; ----------------------------------------------------------------------------

(defun process-monster-diagnostic (params)
  "Process monster diagnostic request with validated parameters.
   
   Parameters:
   -----------
   - params: Property list with validated parameters
     - :language - Language code
     - :favorite-food - User's favorite food
     - :sleep-schedule - User's sleep schedule
     - :hobby - User's hobby
     - :fear - User's greatest fear
   
   Returns:
   --------
   - result: LLM response string, or NIL on error
   
   This function builds the prompt and calls the LLM service."
  (let* ((language (getf params :language))
         (inputs (list :favorite-food (getf params :favorite-food)
                      :sleep-schedule (getf params :sleep-schedule)
                      :hobby (getf params :hobby)
                      :fear (getf params :fear)))
         (prompt (build-monster-diagnostic-prompt inputs language)))
    (call-llm-with-retry prompt :temperature 0.85)))

;;; ----------------------------------------------------------------------------
;;; Public Handlers
;;; ----------------------------------------------------------------------------

(defun handle-monster-diagnostic-request (params)
  "Handles monster diagnostic API requests.
   
   Parameters:
   - params: Alist of request parameters from Ningle
   
   Returns:
   - HTTP response list: (status-code headers body)"
  (handler-case
      (multiple-value-bind (extracted-params error)
          (extract-monster-diagnostic-params params)
        (if error
            (build-validation-error-response error 
                                            (extract-and-validate-language params))
            (let ((result (process-monster-diagnostic extracted-params)))
              (if result
                  `(200 (:content-type "text/html; charset=utf-8")
                    (,(format-monster-diagnostic-response 
                        result 
                        (getf extracted-params :language))))
                  (build-service-error-response 
                    "LLM service unavailable"
                    (getf extracted-params :language))))))
    (error (condition)
      (log-error "monster-diagnostic endpoint" condition)
      (build-service-error-response 
        (format nil "~A" condition)
        (get-param params "language" "en")))))

(defun handle-monster-diagnostic-streaming (params)
  "Handles streaming monster diagnostic API requests with HTMX SSE extension.
   
   Parameters:
   - params: Alist of request parameters from Ningle
   
   Returns:
   - Clack streaming response with SSE format
   
   SSE Event Format (HTMX):
   ------------------------
   Start: event: message\\ndata: <div class=\"alert alert-success\">...\\n\\n
   Content: event: message\\ndata: <span>text</span>\\n\\n
   Done: event: done\\ndata: \\n\\n
   Error: event: error\\ndata: <div class=\"alert alert-error\">...</div>\\n\\n"
  (handler-case
      (multiple-value-bind (extracted-params error)
          (extract-monster-diagnostic-params params)
        (if error
            (build-validation-error-response error 
                                            (extract-and-validate-language params)
                                            :streaming-p t)
            (let* ((language (getf extracted-params :language))
                   (inputs (list :favorite-food (getf extracted-params :favorite-food)
                                :sleep-schedule (getf extracted-params :sleep-schedule)
                                :hobby (getf extracted-params :hobby)
                                :fear (getf extracted-params :fear)))
                   (title (if (string= language "ja") "診断結果" "Diagnosis Result")))
              (create-sse-response
               (lambda (writer)
                 ;; Send start-response event to trigger container creation in JavaScript
                 (funcall writer 
                         (format-sse-event "start-response"
                          (cl-json:encode-json-to-string 
                           (list (cons :feature "monster-diagnostic")
                                 (cons :title title)))))
                 
                 ;; Stream the content chunks
                 (let ((result (call-llm-streaming 
                                (build-monster-diagnostic-prompt inputs language)
                                (lambda (chunk)
                                  (funcall writer 
                                          (format-sse-event "message"
                                           (format nil "<span>~A</span>" (escape-html chunk)))))
                                :temperature 0.85)))
                   
                   (if (eq result :error)
                       ;; Send HTML error alert as 'error' event
                       (funcall writer (format-sse-html-error "LLM service error" language))
                       ;; Send empty 'done' event
                       (funcall writer (format-sse-event "done" "")))))))))
    (error (condition)
      (log-error "monster-diagnostic-streaming endpoint" condition)
      `(500 (:content-type "application/json")
        (,(cl-json:encode-json-to-string
            (list (cons :error (format nil "~A" condition))
                  (cons :type "error"))))))))
