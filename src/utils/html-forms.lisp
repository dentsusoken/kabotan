(in-package :kabotan)

;;; HTML Form Components
;;; This module provides form input components and form wrappers

;;; ============================================================================
;;; Form Input Components
;;; ============================================================================

(defun generate-form-input (&key name label type placeholder required value class id)
  "Generate a DaisyUI form input field
   
   Parameters:
   - name: Input name attribute (required)
   - label: Input label text (optional)
   - type: Input type (default: text)
   - placeholder: Placeholder text (optional)
   - required: Whether field is required (optional)
   - value: Default value (optional)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((input-type (or type "text"))
        (input-id (or id (sanitize-html-id name))))
    (format nil "~
<div class=\"form-control w-full~@[ ~A~]\">~@[
  <label class=\"label\" for=\"~A\">
    <span class=\"label-text\">~A</span>
  </label>~]
  <input type=\"~A\" 
         name=\"~A\" 
         id=\"~A\"~@[
         placeholder=\"~A\"~]~@[
         value=\"~A\"~]~:[~;
         required~]
         class=\"input input-bordered w-full\" />
</div>"
            class
            input-id
            label
            input-type
            (escape-html-attribute name)
            input-id
            (escape-html-attribute placeholder)
            (escape-html-attribute value)
            required)))

(defun generate-form-textarea (&key name label placeholder required value rows class id)
  "Generate a DaisyUI form textarea field
   
   Parameters:
   - name: Textarea name attribute (required)
   - label: Textarea label text (optional)
   - placeholder: Placeholder text (optional)
   - required: Whether field is required (optional)
   - value: Default value (optional)
   - rows: Number of rows (default: 3)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((textarea-id (or id (sanitize-html-id name)))
        (textarea-rows (or rows 3)))
    (format nil "~
<div class=\"form-control w-full~@[ ~A~]\">~@[
  <label class=\"label\" for=\"~A\">
    <span class=\"label-text\">~A</span>
  </label>~]
  <textarea name=\"~A\" 
            id=\"~A\"~@[
            placeholder=\"~A\"~]~:[~;
            required~]
            rows=\"~D\"
            class=\"textarea textarea-bordered w-full\">~@[~A~]</textarea>
</div>"
            class
            textarea-id
            label
            (escape-html-attribute name)
            textarea-id
            (escape-html-attribute placeholder)
            required
            textarea-rows
            (escape-html value))))

(defun generate-form-select (&key name label options required selected class id)
  "Generate a DaisyUI form select dropdown
   
   Parameters:
   - name: Select name attribute (required)
   - label: Select label text (optional)
   - options: List of options as (value . label) pairs (required)
   - required: Whether field is required (optional)
   - selected: Currently selected value (optional)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((select-id (or id (sanitize-html-id name))))
    (format nil "~
<div class=\"form-control w-full~@[ ~A~]\">~@[
  <label class=\"label\" for=\"~A\">
    <span class=\"label-text\">~A</span>
  </label>~]
  <select name=\"~A\" 
          id=\"~A\"~:[~;
          required~]
          class=\"select select-bordered w-full\">
~{    <option value=\"~A\"~:[~; selected~]>~A</option>~%~}  </select>
</div>"
            class
            select-id
            label
            (escape-html-attribute name)
            select-id
            required
            (loop for (value . label-text) in options
                  collect (escape-html-attribute value)
                  collect (equal value selected)
                  collect (escape-html label-text)))))

(defun generate-form-radio-group (&key name label options required selected class id)
  "Generate a DaisyUI form radio button group
   
   Parameters:
   - name: Radio group name attribute (required)
   - label: Radio group label text (optional)
   - options: List of options as (value . label) pairs (required)
   - required: Whether field is required (optional)
   - selected: Currently selected value (optional)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((group-id (or id (sanitize-html-id name))))
    (format nil "~
<div class=\"form-control w-full~@[ ~A~]\"~@[ id=\"~A\"~]>~@[
  <label class=\"label\">
    <span class=\"label-text\">~A</span>
  </label>~]
  <div class=\"flex flex-wrap gap-8\">
~{    <label class=\"cursor-pointer flex items-center gap-3 px-3 min-w-fit\">
      <input type=\"radio\" 
             name=\"~A\" 
             value=\"~A\"~:[~; checked~]~:[~; required~]
             class=\"radio radio-primary\" />
      <span class=\"label-text\">~A</span>
    </label>~%~}  </div>
</div>"
            class
            group-id
            label
            (loop for (value . label-text) in options
                  collect (escape-html-attribute name)
                  collect (escape-html-attribute value)
                  collect (equal value selected)
                  collect required
                  collect (escape-html label-text)))))

(defun generate-button (&key label type class id disabled)
  "Generate a DaisyUI button
   
   Parameters:
   - label: Button label text (required)
   - type: Button type (default: button)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)
   - disabled: Whether button is disabled (optional)"
  (let ((button-type (or type "button")))
    (format nil "~
<button type=\"~A\"~@[ id=\"~A\"~]~:[~; disabled~] class=\"btn btn-primary~@[ ~A~]\">
  ~A
</button>"
            button-type
            id
            disabled
            class
            (escape-html label))))

;;; ============================================================================
;;; Form Wrapper
;;; ============================================================================

(defun generate-form-wrapper (&key action method target swap indicator content class id)
  "Generate an HTMX-enabled form wrapper
   
   Parameters:
   - action: Form action URL (required)
   - method: HTTP method (default: post)
   - target: HTMX target selector (optional)
   - swap: HTMX swap strategy (optional)
   - indicator: Loading indicator selector (optional)
   - content: Form content (required)
   - class: Additional CSS classes (optional)
   - id: HTML id attribute (optional)"
  (let ((http-method (or method "post")))
    (format nil "~
<form hx-~A=\"~A\"~@[
      hx-target=\"~A\"~]~@[
      hx-swap=\"~A\"~]~@[
      hx-indicator=\"~A\"~]~@[
      class=\"~A\"~]~@[
      id=\"~A\"~]>
  ~A
</form>"
            http-method
            action
            target
            swap
            indicator
            class
            id
            content)))
