(in-package :kabotan)

;;; ============================================================================
;;; Handler Utilities Module
;;; ============================================================================
;;;
;;; This module provides common utilities for all API handlers to reduce
;;; code duplication and ensure consistent patterns across the codebase.
;;;
;;; Key Features:
;;; - Parameter extraction and validation
;;; - Consistent error response formatting
;;; - Handler wrapper for uniform error handling
;;; - Support for both streaming and non-streaming handlers
;;;
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Session and Header Extraction
;;; ----------------------------------------------------------------------------

(defun extract-session-id-from-params (params)
  "Extract session ID from request parameters.
   
   This function first checks for Lack's session storage (:lack.session),
   then falls back to cookie-based extraction for backward compatibility.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle (Clack environment)
   
   Returns:
   --------
   - session-id: Session ID string if found, NIL otherwise
   
   Examples:
   ---------
   (extract-session-id-from-params 
     '((:lack.session . #<HASH-TABLE>)))
   => \"session-id-from-lack\"
   
   (extract-session-id-from-params 
     '((:cookie . \"session_id=abc123; other=value\")))
   => \"abc123\"
   
   (extract-session-id-from-params '())
   => NIL"
  ;; First, try to get session ID from Lack's session storage
  (let ((lack-session (cdr (assoc :lack.session params))))
    (if lack-session
        ;; Get or create session ID in Lack's session storage
        (or (gethash "kabotan-session-id" lack-session)
            (let ((new-id (generate-session-id)))
              (setf (gethash "kabotan-session-id" lack-session) new-id)
              new-id))
        ;; Fall back to cookie-based extraction
        (let ((cookie-header (cdr (assoc :cookie params))))
          (when cookie-header
            (extract-session-id-from-cookie cookie-header))))))

(defun extract-language-from-accept-header (params)
  "Extract language preference from Accept-Language header.
   
   This function parses the Accept-Language header and returns the
   first supported language (en or ja). If no supported language is
   found, returns NIL.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle (Clack environment)
   
   Returns:
   --------
   - language: Language code ('en' or 'ja') if found, NIL otherwise
   
   Examples:
   ---------
   (extract-language-from-accept-header 
     '((:accept-language . \"ja,en-US;q=0.9,en;q=0.8\")))
   => \"ja\"
   
   (extract-language-from-accept-header 
     '((:accept-language . \"en-US,en;q=0.9\")))
   => \"en\"
   
   (extract-language-from-accept-header '())
   => NIL"
  ;; Try both :accept-language and :http-accept-language (Clack format)
  (let ((accept-lang (or (cdr (assoc :accept-language params))
                         (cdr (assoc :http-accept-language params)))))
    (when accept-lang
      ;; Parse Accept-Language header (e.g., "ja,en-US;q=0.9,en;q=0.8")
      (let ((languages (cl-ppcre:split "," accept-lang)))
        (dolist (lang languages)
          ;; Extract language code before semicolon or hyphen
          (let* ((lang-code (string-trim '(#\Space) lang))
                 (lang-code (first (cl-ppcre:split ";|\\-" lang-code)))
                 (lang-code (string-downcase lang-code)))
            (when (or (string= lang-code "en")
                      (string= lang-code "ja"))
              (return-from extract-language-from-accept-header lang-code))))))))

(defun detect-language-from-request (params)
  "Detect language from request using multiple sources.
   
   This function implements the language detection priority:
   1. Explicit 'language' parameter in request
   2. Session language preference (if session exists)
   3. Accept-Language header
   4. Default to 'ja'
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - language: Detected language code ('en' or 'ja')
   
   Examples:
   ---------
   (detect-language-from-request 
     '((\"language\" . \"en\")))
   => \"en\"
   
   (detect-language-from-request 
     '((:cookie . \"session_id=abc123\")
       (:accept-language . \"en-US\")))
   => \"en\" (or session language if session exists)"
  ;; 1. Check explicit language parameter
  (let ((explicit-lang (get-param params "language")))
    (when (and explicit-lang (validate-language explicit-lang))
      (return-from detect-language-from-request explicit-lang)))
  
  ;; 2. Check session language preference (only if session exists)
  (let ((session-id (extract-session-id-from-params params)))
    (when session-id
      (let ((session (get-session session-id)))
        (when session
          (let ((session-lang (session-language session)))
            (when session-lang
              (return-from detect-language-from-request session-lang)))))))
  
  ;; 3. Check Accept-Language header
  (let ((header-lang (extract-language-from-accept-header params)))
    (when header-lang
      (return-from detect-language-from-request header-lang)))
  
  ;; 4. Default to Japanese
  "ja")

;;; ----------------------------------------------------------------------------
;;; Parameter Extraction and Validation
;;; ----------------------------------------------------------------------------

(defun extract-and-validate-language (params)
  "Extract and validate language parameter from request params.
   
   This function extracts the 'language' parameter and validates it against
   supported languages ('en' and 'ja'). If the parameter is missing or invalid,
   it defaults to 'en'.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - language: Validated language string ('en' or 'ja')
   - valid-p: T if language was explicitly provided and valid, NIL if defaulted
   
   Examples:
   ---------
   (extract-and-validate-language '((\"language\" . \"ja\")))
   => \"ja\", T
   
   (extract-and-validate-language '((\"language\" . \"invalid\")))
   => \"en\", NIL
   
   (extract-and-validate-language '())
   => \"en\", NIL"
  (let* ((language (get-param params "language" "en"))
         (valid-p (validate-language language)))
    (if valid-p
        (values language t)
        (values "en" nil))))

(defun validate-required-params (params required-keys)
  "Validate that all required parameters are present and non-empty.
   
   This function checks multiple parameters at once, ensuring they exist
   and contain non-empty values. It returns both a validation result and
   a list of missing parameter keys for error reporting.
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   - required-keys: List of parameter key strings to validate
   
   Returns:
   --------
   - valid-p: T if all required parameters are present and non-empty, NIL otherwise
   - missing-keys: List of missing or empty parameter keys (empty list if valid-p is T)
   
   Examples:
   ---------
   (validate-required-params '((\"name\" . \"test\") (\"value\" . \"123\"))
                             '(\"name\" \"value\"))
   => T, NIL
   
   (validate-required-params '((\"name\" . \"\"))
                             '(\"name\" \"value\"))
   => NIL, (\"name\" \"value\")"
  (let ((missing-keys '()))
    (dolist (key required-keys)
      (let ((value (get-param params key)))
        (unless (and value (validate-non-empty-string value))
          (push key missing-keys))))
    (if missing-keys
        (values nil (nreverse missing-keys))
        (values t nil))))

;;; ----------------------------------------------------------------------------
;;; Error Response Builders
;;; ----------------------------------------------------------------------------

(defun build-validation-error-response (message language &key streaming-p retry-endpoint retry-params)
  "Build appropriate error response for validation errors (400 status).
   
   This function creates consistent validation error responses for both
   streaming and non-streaming handlers. Validation errors indicate client
   mistakes (missing parameters, invalid values) and return 400 status.
   
   The response is designed to work with HTMX response-targets extension,
   allowing forms to specify hx-target-4* attributes to direct 4xx errors
   to specific error display containers.
   
   Parameters:
   -----------
   - message: Error message string describing the validation failure
   - language: Language code for error message localization ('en' or 'ja')
   - streaming-p: T for streaming response (JSON), NIL for non-streaming (HTML)
   - retry-endpoint: Optional endpoint URL for retry button (e.g., \"/api/spell-generator\")
   - retry-params: Optional JSON string of parameters for retry (e.g., '{\"language\":\"en\"}')
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)
   
   Response Formats:
   -----------------
   Non-streaming (HTML):
     (400 (:content-type \"text/html; charset=utf-8\") (html-error-string))
   
   Streaming (JSON):
     (400 (:content-type \"application/json\") (json-error-string))
   
   Examples:
   ---------
   (build-validation-error-response \"Missing parameter: name\" \"en\" 
                                    :streaming-p nil
                                    :retry-endpoint \"/api/spell-generator\")
   => (400 (:content-type \"text/html; charset=utf-8\") (\"<div>...</div>\"))
   
   (build-validation-error-response \"パラメータが不足\" \"ja\" :streaming-p t)
   => (400 (:content-type \"application/json\") (\"{\\\"error\\\":...}\"))"
  (if streaming-p
      ;; Streaming response: JSON format
      `(400 (:content-type "application/json")
        (,(cl-json:encode-json-to-string
            (list (cons :error message)
                  (cons :type "error")))))
      ;; Non-streaming response: HTML format
      `(400 (:content-type "text/html; charset=utf-8")
        (,(format-error-response 
            (list :error-type "validation-error"
                  :message message
                  :retry-endpoint retry-endpoint
                  :retry-params retry-params)
            language)))))

(defun build-service-error-response (message language &key streaming-p retry-endpoint retry-params)
  "Build appropriate error response for service errors (500 status).
   
   This function creates consistent service error responses for both
   streaming and non-streaming handlers. Service errors indicate server-side
   issues (LLM API failures, internal errors) and return 500 status.
   
   The response is designed to work with HTMX response-targets extension,
   allowing forms to specify hx-target-5* attributes to direct 5xx errors
   to specific error display containers.
   
   Parameters:
   -----------
   - message: Error message string describing the service failure
   - language: Language code for error message localization ('en' or 'ja')
   - streaming-p: T for streaming response (JSON), NIL for non-streaming (HTML)
   - retry-endpoint: Optional endpoint URL for retry button (e.g., \"/api/spell-generator\")
   - retry-params: Optional JSON string of parameters for retry (e.g., '{\"language\":\"en\"}')
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)
   
   Response Formats:
   -----------------
   Non-streaming (HTML):
     (500 (:content-type \"text/html; charset=utf-8\") (html-error-string))
   
   Streaming (JSON):
     (500 (:content-type \"application/json\") (json-error-string))
   
   Examples:
   ---------
   (build-service-error-response \"LLM service unavailable\" \"en\" 
                                 :streaming-p nil
                                 :retry-endpoint \"/api/spell-generator\")
   => (500 (:content-type \"text/html; charset=utf-8\") (\"<div>...</div>\"))
   
   (build-service-error-response \"LLMサービスエラー\" \"ja\" :streaming-p t)
   => (500 (:content-type \"application/json\") (\"{\\\"error\\\":...}\"))"
  (if streaming-p
      ;; Streaming response: JSON format
      `(500 (:content-type "application/json")
        (,(cl-json:encode-json-to-string
            (list (cons :error message)
                  (cons :type "error")))))
      ;; Non-streaming response: HTML format
      `(500 (:content-type "text/html; charset=utf-8")
        (,(format-error-response 
            (list :error-type "api-error"
                  :message message
                  :retry-endpoint retry-endpoint
                  :retry-params retry-params)
            language)))))

;;; ----------------------------------------------------------------------------
;;; Handler Wrapper
;;; ----------------------------------------------------------------------------

(defun wrap-handler (handler-fn params &key streaming-p)
  "Wrap handler function with consistent error handling.
   
   This function provides a uniform error handling wrapper for all handlers,
   catching unexpected errors and returning appropriate error responses based
   on the handler type (streaming or non-streaming).
   
   The wrapper:
   1. Executes the handler function
   2. Catches any errors that occur
   3. Logs the error with context
   4. Returns an appropriate error response
   
   Parameters:
   -----------
   - handler-fn: Function to execute (should return HTTP response)
   - params: Request parameters (used for language extraction in error responses)
   - streaming-p: T for streaming handler, NIL for non-streaming handler
   
   Returns:
   --------
   - HTTP response from handler-fn on success
   - Error response (500 status) on failure
   
   Error Handling:
   ---------------
   - All errors are caught and logged
   - Language is extracted from params for localized error messages
   - Response format matches handler type (HTML or JSON)
   
   Examples:
   ---------
   (wrap-handler
     (lambda () (process-request params))
     params
     :streaming-p nil)
   
   (wrap-handler
     (lambda () (create-streaming-response params))
     params
     :streaming-p t)"
  (handler-case
      ;; Execute the handler function
      (funcall handler-fn)
    (error (condition)
      ;; Extract language for error message localization
      (let ((language (get-param params "language" "en")))
        ;; Log the error with context
        (log-error (if streaming-p
                       "streaming handler"
                       "non-streaming handler")
                   condition)
        ;; Return appropriate error response
        (build-service-error-response
         (format nil "~A" condition)
         language
         :streaming-p streaming-p)))))
