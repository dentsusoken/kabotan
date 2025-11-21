(in-package :kabotan)

;;; Error handling utilities

(defvar *suppress-error-logs* nil
  "When T, suppresses error logs (useful for testing)")

(defun handle-api-error (error)
  "Handle API errors and return appropriate error information"
  (unless *suppress-error-logs*
    (format *error-output* "API Error: ~A~%" error))
  (list :error-type "api-error"
        :message (format nil "~A" error)))

(defun format-error-response (error-info language)
  "Format error information as HTML response with retry button.
   
   This function generates an HTML error alert with a localized error message
   and an optional retry button. The error message is extracted from error-info
   or uses a default message based on the error type.
   
   Parameters:
   -----------
   - error-info: Property list containing error information
                 Keys: :error-type, :message, :retry-endpoint, :retry-params
   - language: Language code ('en' or 'ja') for message localization
   
   Returns:
   --------
   - HTML string containing error alert with retry button
   
   Examples:
   ---------
   (format-error-response 
     '(:error-type \"validation-error\" 
       :message \"Missing required field\")
     \"en\")
   => \"<div class=\\\"alert alert-error\\\">...</div>\"
   
   (format-error-response 
     '(:error-type \"api-error\" 
       :message \"LLM service unavailable\"
       :retry-endpoint \"/api/spell-generator\"
       :retry-params \"language=en\")
     \"en\")
   => \"<div class=\\\"alert alert-error\\\">...<button>Retry</button></div>\""
  (let* ((error-type (getf error-info :error-type))
         (custom-message (getf error-info :message))
         (retry-endpoint (getf error-info :retry-endpoint))
         (retry-params (getf error-info :retry-params))
         (default-message (if (string= language "ja")
                             "エラーが発生しました。もう一度お試しください。"
                             "An error occurred. Please try again."))
         (message (or custom-message default-message))
         (retry-label (if (string= language "ja") "再試行" "Retry")))
    (format nil "<div class=\"alert alert-error\">
  <div class=\"flex flex-col gap-2 w-full\">
    <span>~A</span>~@[
    <button class=\"btn btn-sm btn-primary\" 
            hx-post=\"~A\"~@[
            hx-vals='~A'~]
            hx-target=\"#feature-content\"
            hx-swap=\"innerHTML\">
      ~A
    </button>~]
  </div>
</div>"
            (escape-html message)
            retry-endpoint
            retry-params
            retry-label)))

(defun log-error (context error)
  "Log error with context information"
  (unless *suppress-error-logs*
    (format *error-output* "[ERROR] ~A: ~A~%" context error)))
