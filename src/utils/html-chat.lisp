(in-package :kabotan)

;;; HTML Chat Components
;;; This module provides chat and conversation UI components

;;; ============================================================================
;;; Chat Message Components
;;; ============================================================================

(defun generate-chat-message (role content character language)
  "Generate HTML for a single chat message
   
   Parameters:
   - role: Message role (\"user\" or \"assistant\")
   - content: Message content text
   - character: Character name (for assistant messages)
   - language: Language code (\"ja\" or \"en\")
   
   Returns:
   - HTML string containing a chat bubble"
  (let ((is-user (string= role "user"))
        (character-label (if (string= role "assistant")
                            (get-character-display-name character language)
                            (get-ui-text :chat-message language))))
    (format nil "~
<div class=\"chat ~A\">
  <div class=\"chat-header\">
    ~A
  </div>
  <div class=\"chat-bubble ~A\">
    ~A
  </div>
</div>"
            (if is-user "chat-end" "chat-start")
            (escape-html character-label)
            (if is-user "chat-bubble-primary" "chat-bubble-secondary")
            (escape-html content))))

(defun get-character-display-name (character language)
  "Get the display name for a character in the specified language
   
   Parameters:
   - character: Character identifier (\"dracula\", \"witch\", \"jack\")
   - language: Language code (\"ja\" or \"en\")
   
   Returns:
   - Localized character display name"
  (cond
    ((string= character "dracula") 
     (get-ui-text :chat-character-dracula language))
    ((string= character "witch") 
     (get-ui-text :chat-character-witch language))
    ((string= character "jack") 
     (get-ui-text :chat-character-jack language))
    (t character)))

(defun generate-character-chat-streaming-form (description character-selector history-html language message-input submit-button loading-indicator)
  "Generate streaming version of character chat form
   
   Parameters:
   - description: Feature description text
   - character-selector: Character selection radio group HTML
   - history-html: Conversation history HTML
   - language: Language code
   - message-input: Message input field HTML
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   
   Returns:
   - HTML string for streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"character-chat-content\">
      <div id=\"chat-history-container\" 
           class=\"space-y-2 mb-4 max-h-96 overflow-y-auto\"
           hx-ext=\"sse\"
           sse-swap=\"message\"
           hx-swap=\"beforeend\">
        ~A
      </div>
      <form hx-get=\"/api/character-chat-stream\"
            hx-target=\"#chat-history-container\"
            hx-swap=\"beforeend\"
            hx-indicator=\"#chat-loading\"
            hx-on:htmx:after-request=\"this.reset()\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
    </div>"
          description
          history-html
          language
          character-selector
          message-input
          submit-button
          loading-indicator))

(defun generate-character-chat-non-streaming-form (description character-selector history-html language message-input submit-button loading-indicator)
  "Generate non-streaming version of character chat form
   
   Parameters:
   - description: Feature description text
   - character-selector: Character selection radio group HTML
   - history-html: Conversation history HTML
   - language: Language code
   - message-input: Message input field HTML
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   
   Returns:
   - HTML string for non-streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"character-chat-content\">
      <div id=\"chat-history-container\" class=\"space-y-2 mb-4 max-h-96 overflow-y-auto\">
        ~A
      </div>
      <form hx-post=\"/api/character-chat\"
            hx-target=\"#chat-history-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"beforeend\"
            hx-indicator=\"#chat-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
    </div>"
          description
          history-html
          language
          character-selector
          message-input
          submit-button
          loading-indicator))

(defun generate-character-chat-form (language &optional history character use-streaming)
  "Generate HTML form for character chat feature with conversation history
   
   Parameters:
   - language: Language code (\"ja\" or \"en\")
   - history: Optional list of conversation-message structs
   - character: Currently selected character (default: \"dracula\")
   - use-streaming: If t, use HTMX SSE extension for streaming (default: nil)
   
   Returns:
   - HTML string containing the complete character chat interface with HTMX attributes"
  (let* ((title (get-ui-text :chat-title language))
         (description (get-ui-text :chat-description language))
         (character-label (get-ui-text :chat-character language))
         (character-dracula (get-ui-text :chat-character-dracula language))
         (character-witch (get-ui-text :chat-character-witch language))
         (character-jack (get-ui-text :chat-character-jack language))
         (message-label (get-ui-text :chat-message language))
         (submit-label (get-ui-text :chat-submit language))
         (loading-text (get-ui-text :loading language))
         (selected-character (or character "dracula"))
         (history-html (if history
                          (with-output-to-string (out)
                            (dolist (msg history)
                              (write-string 
                               (generate-chat-message 
                                (conversation-message-role msg)
                                (conversation-message-content msg)
                                selected-character
                                language)
                               out)))
                          ""))
         (character-selector (generate-form-radio-group
                              :name "character"
                              :label character-label
                              :options (list (cons "dracula" character-dracula)
                                            (cons "witch" character-witch)
                                            (cons "jack-o-lantern" character-jack))
                              :required t
                              :selected selected-character
                              :id "character-selection"
                              :class "mb-4"))
         (message-input (generate-form-input
                         :name "message"
                         :label message-label
                         :type "text"
                         :required t
                         :id "chat-message-input"))
         (submit-button (generate-button 
                         :label submit-label
                         :type "submit"
                         :id "chat-submit-btn"
                         :class "btn-primary"))
         (loading-indicator (generate-loading-indicator 
                             :id "chat-loading"
                             :message loading-text)))
    (generate-card
     :title title
     :id "character-chat-card"
     :content (if use-streaming
                  (generate-character-chat-streaming-form
                   description character-selector history-html language
                   message-input submit-button loading-indicator)
                  (generate-character-chat-non-streaming-form
                   description character-selector history-html language
                   message-input submit-button loading-indicator)))))

(defun generate-trivia-message (role content language)
  "Generate HTML for a single trivia message
   
   Parameters:
   - role: Message role (\"user\" or \"assistant\")
   - content: Message content text
   - language: Language code (\"ja\" or \"en\")
   
   Returns:
   - HTML string containing a chat bubble"
  (let ((is-user (string= role "user"))
        (role-label (if (string= role "assistant")
                       "Trivia Bot"
                       (get-ui-text :trivia-question language))))
    (format nil "~
<div class=\"chat ~A\">
  <div class=\"chat-header\">
    ~A
  </div>
  <div class=\"chat-bubble ~A\">
    ~A
  </div>
</div>"
            (if is-user "chat-end" "chat-start")
            (escape-html role-label)
            (if is-user "chat-bubble-primary" "chat-bubble-secondary")
            (escape-html content))))

(defun generate-trivia-bot-streaming-form (description history-html language question-input submit-button loading-indicator)
  "Generate streaming version of trivia bot form
   
   Parameters:
   - description: Feature description text
   - history-html: Conversation history HTML
   - language: Language code
   - question-input: Question input field HTML
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   
   Returns:
   - HTML string for streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"trivia-bot-content\">
      <div id=\"trivia-history-container\" 
           class=\"space-y-2 mb-4 max-h-96 overflow-y-auto\"
           hx-ext=\"sse\"
           sse-swap=\"message\"
           hx-swap=\"beforeend\">
        ~A
      </div>
      <form hx-get=\"/api/trivia-bot-stream\"
            hx-target=\"#trivia-history-container\"
            hx-swap=\"beforeend\"
            hx-indicator=\"#trivia-loading\"
            hx-on:htmx:after-request=\"this.reset()\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
    </div>"
          description
          history-html
          language
          question-input
          submit-button
          loading-indicator))

(defun generate-trivia-bot-non-streaming-form (description history-html language question-input submit-button loading-indicator)
  "Generate non-streaming version of trivia bot form
   
   Parameters:
   - description: Feature description text
   - history-html: Conversation history HTML
   - language: Language code
   - question-input: Question input field HTML
   - submit-button: Submit button HTML
   - loading-indicator: Loading indicator HTML
   
   Returns:
   - HTML string for non-streaming form"
  (format nil "~
    <p class=\"text-base-content/70 mb-4\">~A</p>
    <div id=\"trivia-bot-content\">
      <div id=\"trivia-history-container\" class=\"space-y-2 mb-4 max-h-96 overflow-y-auto\">
        ~A
      </div>
      <form hx-post=\"/api/trivia-bot\"
            hx-target=\"#trivia-history-container\"
            hx-target-4*=\"#error-display\"
            hx-target-5*=\"#error-display\"
            hx-swap=\"beforeend\"
            hx-indicator=\"#trivia-loading\"
            class=\"space-y-4\">
        <input type=\"hidden\" name=\"language\" value=\"~A\" />
        ~A
        <div class=\"flex items-center gap-4\">
          ~A
          ~A
        </div>
      </form>
    </div>"
          description
          history-html
          language
          question-input
          submit-button
          loading-indicator))

(defun generate-trivia-bot-form (language &optional history &key streaming-p)
  "Generate HTML form for trivia bot feature with conversation history
   
   Parameters:
   - language: Language code (\"ja\" or \"en\")
   - history: Optional list of conversation-message structs
   - streaming-p: Whether to use streaming mode (optional)
   
   Returns:
   - HTML string containing the complete trivia bot interface with HTMX attributes"
  (let* ((title (get-ui-text :trivia-title language))
         (description (get-ui-text :trivia-description language))
         (question-label (get-ui-text :trivia-question language))
         (submit-label (get-ui-text :trivia-submit language))
         (loading-text (get-ui-text :loading language))
         (history-html (if history
                          (with-output-to-string (out)
                            (dolist (msg history)
                              (write-string 
                               (generate-trivia-message 
                                (conversation-message-role msg)
                                (conversation-message-content msg)
                                language)
                               out)))
                          ""))
         (question-input (generate-form-input
                          :name "question"
                          :label question-label
                          :type "text"
                          :required t
                          :id "trivia-question-input"))
         (submit-button (generate-button 
                         :label submit-label
                         :type "submit"
                         :id "trivia-submit-btn"
                         :class "btn-primary"))
         (loading-indicator (generate-loading-indicator 
                             :id "trivia-loading"
                             :message loading-text)))
    (generate-card
     :title title
     :id "trivia-bot-card"
     :content (if streaming-p
                  (generate-trivia-bot-streaming-form
                   description history-html language
                   question-input submit-button loading-indicator)
                  (generate-trivia-bot-non-streaming-form
                   description history-html language
                   question-input submit-button loading-indicator)))))
