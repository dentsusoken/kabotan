(in-package :kabotan)

;;; HTML Feature-Specific Forms
;;; This module provides feature-specific form generators for spell, monster, and story features

;;; ============================================================================
;;; Feature-Specific Form Generators
;;; ============================================================================

(defun generate-spell-generator-form (language &key streaming-p)
  "Generate HTML form for spell generator feature
   
   Parameters:
   - language: Language code (\"ja\" or \"en\")
   - streaming-p: Whether to use streaming mode (optional)
   
   Returns:
   - HTML string containing the complete spell generator form with HTMX attributes
   
   The form includes:
   - Feature title and description
   - Generate button with HTMX attributes
   - Loading indicator (automatically shown/hidden during requests)
   - Result container for displaying generated spell
   - Streaming support via HTMX SSE extension (if streaming-p is true)
   
   HTMX Attributes Pattern:
   - hx-post/hx-get: Specifies the API endpoint
   - hx-target: Specifies where to insert the response (#spell-result-container)
   - hx-swap: Controls how content is swapped (innerHTML for complete replacement)
   - hx-indicator: Links to the loading indicator element (#spell-loading)
   - hx-target-4*/hx-target-5*: Error handling targets for 4xx/5xx responses
   
   Content Replacement Behavior:
   - Uses innerHTML swap to completely replace previous content
   - Result container starts empty to ensure clean slate
   - Each regeneration replaces the entire content, not appends"
  (let ((title (get-ui-text :spell-title language))
        (description (get-ui-text :spell-description language))
        (regenerate-label (get-ui-text :spell-regenerate language))
        (loading-text (get-ui-text :loading language)))
    (generate-card
     :title title
     :id "spell-generator-card"
     :content (if streaming-p
                  ;; Streaming version with SSE
                  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"spell-generator-content\">
      <form hx-get=\"/api/spell-generator-stream\"
            hx-target=\"#spell-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#spell-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      <div id=\"spell-result-container\" 
           class=\"mt-4 streaming-content\"
           data-placeholder=\"~A\">
      </div>
    </div>"
                          description
                          language
                          (generate-button 
                           :label regenerate-label
                           :type "submit"
                           :id "spell-regenerate-btn"
                           :class "btn-primary")
                          (generate-loading-indicator 
                           :id "spell-loading"
                           :message loading-text)
                          (get-ui-text :loading language))
                  ;; Non-streaming version
                  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"spell-generator-content\">
      <form hx-post=\"/api/spell-generator\"
            hx-target=\"#spell-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#spell-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      ~A
    </div>"
                          description
                          language
                          (generate-button 
                           :label regenerate-label
                           :type "submit"
                           :id "spell-regenerate-btn"
                           :class "btn-primary")
                          (generate-loading-indicator 
                           :id "spell-loading"
                           :message loading-text)
                          (generate-result-container 
                           :id "spell-result-container"
                           :class "mt-4"))))))

(defun generate-monster-diagnostic-streaming-form (description language form-inputs submit-button loading-indicator loading-placeholder)
  "Generate streaming version of monster diagnostic form
   
   Parameters:
   - description: Feature description text
   - language: Language code
   - form-inputs: HTML string containing all form input fields
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   - loading-placeholder: Loading placeholder text
   
   Returns:
   - HTML string for streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"monster-diagnostic-content\">
      <form hx-get=\"/api/monster-diagnostic-stream\"
            hx-target=\"#monster-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#monster-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      <div id=\"monster-result-container\" 
           class=\"mt-4 streaming-content\"
           hx-ext=\"sse\"
           sse-swap=\"message\"
           data-placeholder=\"~A\">
      </div>
    </div>"
          description
          language
          form-inputs
          submit-button
          loading-indicator
          loading-placeholder))

(defun generate-monster-diagnostic-non-streaming-form (description language form-inputs submit-button loading-indicator result-container)
  "Generate non-streaming version of monster diagnostic form
   
   Parameters:
   - description: Feature description text
   - language: Language code
   - form-inputs: HTML string containing all form input fields
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   - result-container: Result container HTML
   
   Returns:
   - HTML string for non-streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"monster-diagnostic-content\">
      <form hx-post=\"/api/monster-diagnostic\"
            hx-target=\"#monster-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#monster-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      ~A
    </div>"
          description
          language
          form-inputs
          submit-button
          loading-indicator
          result-container))

(defun generate-monster-diagnostic-form (language &key streaming-p)
  "Generate HTML form for monster diagnostic feature
   
   Parameters:
   - language: Language code (\"ja\" or \"en\")
   - streaming-p: Whether to use streaming mode (optional)
   
   Returns:
   - HTML string containing the complete monster diagnostic form with HTMX attributes"
  (let* ((title (get-ui-text :monster-diagnostic-title language))
         (description (get-ui-text :monster-diagnostic-description language))
         (favorite-food-label (get-ui-text :monster-favorite-food language))
         (sleep-schedule-label (get-ui-text :monster-sleep-schedule language))
         (hobby-label (get-ui-text :monster-hobby language))
         (fear-label (get-ui-text :monster-fear language))
         (submit-label (get-ui-text :monster-submit language))
         (loading-text (get-ui-text :loading language))
         (form-inputs (with-output-to-string (out)
                       (write-string (generate-form-input
                                      :name "favorite_food"
                                      :label favorite-food-label
                                      :type "text"
                                      :required t
                                      :id "monster-favorite-food") out)
                       (write-string (generate-form-input
                                      :name "sleep_schedule"
                                      :label sleep-schedule-label
                                      :type "text"
                                      :required t
                                      :id "monster-sleep-schedule") out)
                       (write-string (generate-form-input
                                      :name "hobby"
                                      :label hobby-label
                                      :type "text"
                                      :required t
                                      :id "monster-hobby") out)
                       (write-string (generate-form-input
                                      :name "fear"
                                      :label fear-label
                                      :type "text"
                                      :required t
                                      :id "monster-fear") out)))
         (submit-button (generate-button 
                         :label submit-label
                         :type "submit"
                         :id "monster-submit-btn"
                         :class "btn-primary"))
         (loading-indicator (generate-loading-indicator 
                             :id "monster-loading"
                             :message loading-text)))
    (generate-card
     :title title
     :id "monster-diagnostic-card"
     :content (if streaming-p
                  (generate-monster-diagnostic-streaming-form
                   description language form-inputs submit-button
                   loading-indicator (get-ui-text :loading language))
                  (generate-monster-diagnostic-non-streaming-form
                   description language form-inputs submit-button
                   loading-indicator (generate-result-container 
                                      :id "monster-result-container"
                                      :class "mt-4"))))))

(defun generate-story-generator-streaming-form (description language form-inputs submit-button loading-indicator loading-placeholder)
  "Generate streaming version of story generator form
   
   Parameters:
   - description: Feature description text
   - language: Language code
   - form-inputs: HTML string containing all form input fields
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   - loading-placeholder: Loading placeholder text
   
   Returns:
   - HTML string for streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"story-generator-content\">
      <form hx-get=\"/api/story-generator-stream\"
            hx-target=\"#story-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#story-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      <div id=\"story-result-container\" 
           class=\"mt-4 streaming-content\"
           data-placeholder=\"~A\">
      </div>
    </div>"
          description
          language
          form-inputs
          submit-button
          loading-indicator
          loading-placeholder))

(defun generate-story-generator-non-streaming-form (description language form-inputs submit-button loading-indicator result-container)
  "Generate non-streaming version of story generator form
   
   Parameters:
   - description: Feature description text
   - language: Language code
   - form-inputs: HTML string containing all form input fields
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   - result-container: Result container HTML
   
   Returns:
   - HTML string for non-streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"story-generator-content\">
      <form hx-post=\"/api/story-generator\"
            hx-target=\"#story-result-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"innerHTML\"
            hx-indicator=\"#story-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
      ~A
    </div>"
          description
          language
          form-inputs
          submit-button
          loading-indicator
          result-container))

(defun generate-story-generator-form (language &key streaming-p)
  "Generate HTML form for story generator feature
   
   Parameters:
   - language: Language code (\"ja\" or \"en\")
   - streaming-p: Whether to use streaming mode (optional)
   
   Returns:
   - HTML string containing the complete story generator form with HTMX attributes"
  (let* ((title (get-ui-text :story-title language))
         (description (get-ui-text :story-description language))
         (name-label (get-ui-text :story-name language))
         (theme-label (get-ui-text :story-theme language))
         (style-label (get-ui-text :story-style language))
         (style-gothic (get-ui-text :story-style-gothic language))
         (style-parody (get-ui-text :story-style-parody language))
         (style-classic (get-ui-text :story-style-classic language))
         (submit-label (get-ui-text :story-submit language))
         (loading-text (get-ui-text :loading language))
         (form-inputs (with-output-to-string (out)
                       (write-string (generate-form-input
                                      :name "name"
                                      :label name-label
                                      :type "text"
                                      :required t
                                      :id "story-name") out)
                       (write-string (generate-form-input
                                      :name "theme"
                                      :label theme-label
                                      :type "text"
                                      :required t
                                      :id "story-theme") out)
                       (write-string (generate-form-radio-group
                                      :name "style"
                                      :label style-label
                                      :options (list (cons "gothic" style-gothic)
                                                    (cons "parody" style-parody)
                                                    (cons "classic" style-classic))
                                      :required t
                                      :selected "gothic"
                                      :id "story-style") out)))
         (submit-button (generate-button 
                         :label submit-label
                         :type "submit"
                         :id "story-submit-btn"
                         :class "btn-primary"))
         (loading-indicator (generate-loading-indicator 
                             :id "story-loading"
                             :message loading-text)))
    (generate-card
     :title title
     :id "story-generator-card"
     :content (if streaming-p
                  (generate-story-generator-streaming-form
                   description language form-inputs submit-button
                   loading-indicator (get-ui-text :loading language))
                  (generate-story-generator-non-streaming-form
                   description language form-inputs submit-button
                   loading-indicator (generate-result-container 
                                      :id "story-result-container"
                                      :class "mt-4"))))))
