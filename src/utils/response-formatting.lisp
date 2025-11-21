(in-package :kabotan)

;;; HTML response formatting utilities

(defun escape-html (text)
  "Escape HTML special characters in text.
   
   Parameters:
   - text: String to escape
   
   Returns:
   - Escaped string safe for HTML output"
  (if (null text)
      ""
      (let ((result text))
        (setf result (cl-ppcre:regex-replace-all "&" result "&amp;"))
        (setf result (cl-ppcre:regex-replace-all "<" result "&lt;"))
        (setf result (cl-ppcre:regex-replace-all ">" result "&gt;"))
        (setf result (cl-ppcre:regex-replace-all "\"" result "&quot;"))
        (setf result (cl-ppcre:regex-replace-all "'" result "&#39;"))
        result)))

(defun format-html-response (content &key (class "card bg-base-200 shadow-xl"))
  "Format content as HTML with DaisyUI styling"
  (format nil "<div class=\"~A\"><div class=\"card-body\">~A</div></div>" class content))

(defun format-monster-diagnostic-response (result language)
  "Format monster diagnostic result as HTML"
  (let ((title (if (string= language "ja") "診断結果" "Diagnosis Result")))
    (format nil "<div class=\"alert alert-success\">
  <h3 class=\"font-bold text-lg mb-2\">~A</h3>
  <div class=\"whitespace-pre-wrap\">~A</div>
</div>" title (escape-html result))))

(defun format-story-response (story language)
  "Format story generation result as HTML"
  (let ((title (if (string= language "ja") "あなたのストーリー" "Your Story")))
    (format nil "<div class=\"bg-base-300 rounded-lg p-4\">
  <h3 class=\"font-bold text-lg mb-2\">~A</h3>
  <div class=\"whitespace-pre-wrap\">~A</div>
</div>" title (escape-html story))))

(defun format-character-chat-response-with-history (user-message assistant-message character language)
  "Format character chat with both user and assistant messages as HTML.
   
   This function generates HTML for a complete chat exchange including:
   - User message (chat-end, primary bubble)
   - Assistant response (chat-start, secondary bubble)
   
   Parameters:
   -----------
   - user-message: The user's message text
   - assistant-message: The assistant's response text
   - character: Character name (dracula, witch, jack)
   - language: Language code (ja or en)
   
   Returns:
   --------
   - HTML string containing both chat bubbles"
  (let ((user-label (get-ui-text :chat-message language))
        (character-label (cond
                          ((string= character "dracula") 
                           (get-ui-text :chat-character-dracula language))
                          ((string= character "witch") 
                           (get-ui-text :chat-character-witch language))
                          ((string= character "jack") 
                           (get-ui-text :chat-character-jack language))
                          (t character))))
    (format nil "~
<div class=\"chat chat-end\">
  <div class=\"chat-header\">~A</div>
  <div class=\"chat-bubble chat-bubble-primary\">~A</div>
</div>
<div class=\"chat chat-start\">
  <div class=\"chat-header\">~A</div>
  <div class=\"chat-bubble chat-bubble-secondary\">~A</div>
</div>"
            (escape-html user-label)
            (escape-html user-message)
            (escape-html character-label)
            (escape-html assistant-message))))



(defun format-trivia-response-with-history (user-question assistant-answer language)
  "Format trivia bot with both user and assistant messages as HTML.
   
   This function generates HTML for a complete trivia exchange including:
   - User question (chat-end, primary bubble)
   - Assistant answer (chat-start, secondary bubble)
   
   Parameters:
   -----------
   - user-question: The user's question text
   - assistant-answer: The assistant's answer text
   - language: Language code (ja or en)
   
   Returns:
   --------
   - HTML string containing both chat bubbles"
  (let ((user-label (get-ui-text :trivia-question language)))
    (format nil "~
<div class=\"chat chat-end\">
  <div class=\"chat-header\">~A</div>
  <div class=\"chat-bubble chat-bubble-primary\">~A</div>
</div>
<div class=\"chat chat-start\">
  <div class=\"chat-header\">🎓 Trivia Bot</div>
  <div class=\"chat-bubble chat-bubble-secondary\">~A</div>
</div>"
            (escape-html user-label)
            (escape-html user-question)
            (escape-html assistant-answer))))

(defun format-spell-generator-response (spell language)
  "Format spell generator response as HTML"
  (declare (ignore language))
  ;; Parse spell and meaning from response
  ;; Support both plain text and markdown formats
  (let* ((spell-markers '("**呪文:**" "呪文:" "**Spell:**" "Spell:"))
         (meaning-markers '("**意味:**" "意味:" "**Meaning:**" "Meaning:"))
         (spell-start nil)
         (spell-marker-len 0)
         (meaning-start nil)
         (meaning-marker-len 0))
    
    ;; Find spell marker
    (dolist (marker spell-markers)
      (let ((pos (search marker spell)))
        (when pos
          (setf spell-start pos)
          (setf spell-marker-len (length marker))
          (return))))
    
    ;; Find meaning marker
    (dolist (marker meaning-markers)
      (let ((pos (search marker spell)))
        (when pos
          (setf meaning-start pos)
          (setf meaning-marker-len (length marker))
          (return))))
    
    ;; Extract text
    (let ((spell-text (if (and spell-start meaning-start)
                         (string-trim '(#\Space #\Newline #\Return #\*)
                                     (subseq spell 
                                            (+ spell-start spell-marker-len)
                                            meaning-start))
                         ""))
          (meaning-text (if meaning-start
                           (string-trim '(#\Space #\Newline #\Return)
                                       (subseq spell 
                                              (+ meaning-start meaning-marker-len)))
                           "")))
      (format nil "<div class=\"text-center\">
  <div class=\"spell-phrase\">~A</div>
  <div class=\"spell-explanation mt-6\">~A</div>
</div>" (escape-html spell-text) (escape-html meaning-text)))))
