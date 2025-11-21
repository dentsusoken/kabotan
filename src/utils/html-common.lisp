(in-package :kabotan)

;;; HTML Common Utilities
;;; This module provides HTML escaping functions and basic DaisyUI components

;;; ============================================================================
;;; HTML Escaping and Sanitization
;;; ============================================================================

(defun escape-html-char (char)
  "Escape a single HTML character"
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\" "&quot;")
    (#\' "&#39;")
    (t (string char))))

(defun escape-html (text)
  "Escape text for use in HTML content"
  (when text
    (with-output-to-string (out)
      (loop for char across (princ-to-string text)
            do (write-string (escape-html-char char) out)))))

;;; ============================================================================
;;; HTML Attribute Escaping
;;; ============================================================================

(defun escape-html-attribute (text)
  "Escape text for use in HTML attributes"
  (when text
    (with-output-to-string (out)
      (loop for char across (princ-to-string text)
            do (write-string (escape-html-char char) out)))))

(defun sanitize-html-id (text)
  "Sanitize text to create a valid HTML ID"
  (when text
    (cl-ppcre:regex-replace-all 
     "[^a-zA-Z0-9-_]" 
     (string-downcase (princ-to-string text))
     "-")))

;;; ============================================================================
;;; DaisyUI Component Wrappers
;;; ============================================================================

(defun generate-card (&key title content class id)
  "Generate a DaisyUI card component
   
   Parameters:
   - title: Card title (optional)
   - content: Card body content (required)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (format nil "~
<div class=\"card bg-base-200 shadow-xl~@[ ~A~]\"~@[ id=\"~A\"~]>
  <div class=\"card-body\">~@[
    <h2 class=\"card-title\">~A</h2>~]
    ~A
  </div>
</div>"
          class
          id
          title
          content))

(defun generate-alert (&key type message class id)
  "Generate a DaisyUI alert component
   
   Parameters:
   - type: Alert type (:info :success :warning :error) (required)
   - message: Alert message content (required)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((alert-class (case type
                       (:info "alert-info")
                       (:success "alert-success")
                       (:warning "alert-warning")
                       (:error "alert-error")
                       (t "alert-info"))))
    (format nil "~
<div class=\"alert ~A~@[ ~A~]\"~@[ id=\"~A\"~]>
  <span>~A</span>
</div>"
            alert-class
            class
            id
            message)))

(defun generate-loading-indicator (&key id class message)
  "Generate a loading indicator with htmx-indicator class
   
   This function creates a loading indicator that works with HTMX's automatic
   request state management. The indicator is hidden by default and automatically
   shown during HTMX requests.
   
   HTMX Integration:
   - The indicator has class 'htmx-indicator' which is hidden by default (CSS)
   - When HTMX makes a request, it adds 'htmx-request' class to the requesting element
   - CSS rule '.htmx-request .htmx-indicator' makes the indicator visible
   - The indicator automatically hides when the request completes
   
   Usage Pattern:
   1. Create a loading indicator with this function
   2. Add hx-indicator=\"#indicator-id\" to the form or button
   3. The indicator will show/hide automatically during requests
   
   Example:
   (generate-loading-indicator :id \"my-loading\" :message \"Processing...\")
   ;; In form: hx-indicator=\"#my-loading\"
   
   Parameters:
   - id: HTML id attribute (optional, but recommended for hx-indicator targeting)
   - class: Additional CSS classes (optional)
   - message: Loading message (default: Loading...)"
  (format nil "~
<span class=\"htmx-indicator~@[ ~A~]\"~@[ id=\"~A\"~]>
  <span class=\"loading loading-spinner loading-md\"></span>
  ~@[<span class=\"ml-2 text-base-content\">~A</span>~]
</span>"
          class
          id
          (escape-html (or message "Loading..."))))

(defun generate-container (&key content class id)
  "Generate a generic container div
   
   Parameters:
   - content: Container content (required)
   - class: CSS classes (optional)
   - id: HTML id attribute (optional)"
  (format nil "~
<div~@[ class=\"~A\"~]~@[ id=\"~A\"~]>
  ~A
</div>"
          class
          id
          content))

(defun generate-error-display (&key id class)
  "Generate an empty error display container
   
   Parameters:
   - id: HTML id attribute (default: error-display)
   - class: Additional CSS classes (optional)"
  (format nil "~
<div id=\"~A\" class=\"~@[~A ~]hidden\"></div>"
          (or id "error-display")
          class))

(defun generate-result-container (&key id class)
  "Generate an empty result container
   
   Parameters:
   - id: HTML id attribute (default: result-container)
   - class: Additional CSS classes (optional)"
  (format nil "~
<div id=\"~A\"~@[ class=\"~A\"~]></div>"
          (or id "result-container")
          class))
