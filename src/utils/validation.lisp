(in-package :kabotan)

;;; Input validation functions

(defun validate-language (language)
  "Validate that language is either 'ja' or 'en'"
  (member language '("ja" "en") :test #'string=))

(defun validate-non-empty-string (str)
  "Validate that string is not empty or nil"
  (and str (stringp str) (> (length (string-trim '(#\Space #\Tab #\Newline) str)) 0)))

(defun validate-story-style (style)
  "Validate that story style is one of the allowed values"
  (member style '("gothic" "parody" "classic") :test #'string=))

(defun sanitize-input (input)
  "Sanitize user input to prevent injection attacks"
  (when (and input (stringp input))
    ;; Remove potentially dangerous characters and limit length
    (let ((cleaned (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
      (if (> (length cleaned) 5000)
          (subseq cleaned 0 5000)
          cleaned))))

(defun get-param (params key &optional default)
  "Helper function to safely get parameter value from params alist.
   
   Parameters:
   - params: Alist of request parameters
   - key: Parameter key to look up (string or keyword)
   - default: Default value if parameter is not found or empty
   
   Returns:
   - Parameter value or default
   
   Note: Supports both string keys and keyword symbols for compatibility
   with different parameter formats (POST body vs GET query params)"
  (let ((value (or (cdr (assoc key params :test #'string=))
                   (cdr (assoc key params :test #'string-equal))
                   (cdr (assoc (intern (string-upcase key) :keyword) params)))))
    (if (and value (stringp value) (> (length value) 0))
        value
        default)))


