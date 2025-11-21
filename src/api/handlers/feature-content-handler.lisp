(in-package :kabotan)

;;; ============================================================================
;;; Feature Content API Handler
;;; ============================================================================
;;;
;;; This handler serves initial HTML forms for each feature.
;;; It implements the HTMX-driven architecture where the server generates
;;; complete HTML forms that are loaded dynamically.
;;;
;;; Endpoints:
;;; - GET /api/features/monster-diagnostic
;;; - GET /api/features/story-generator
;;; - GET /api/features/character-chat
;;; - GET /api/features/trivia-bot
;;; - GET /api/features/spell-generator
;;;
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Feature Form Generators
;;; ----------------------------------------------------------------------------
;;; Form generators are defined in html-templates.lisp:
;;; - generate-monster-diagnostic-form
;;; - generate-story-generator-form
;;; - generate-character-chat-form
;;; - generate-trivia-bot-form
;;; - generate-spell-generator-form

;;; ----------------------------------------------------------------------------
;;; Feature Name Mapping
;;; ----------------------------------------------------------------------------

(defparameter *feature-generators*
  '(("monster-diagnostic" . generate-monster-diagnostic-form)
    ("story-generator" . generate-story-generator-form)
    ("character-chat" . generate-character-chat-form)
    ("trivia-bot" . generate-trivia-bot-form)
    ("spell-generator" . generate-spell-generator-form))
  "Mapping of feature names to their form generator functions")

;;; ----------------------------------------------------------------------------
;;; Base Feature Content Handler
;;; ----------------------------------------------------------------------------

(defun handle-feature-content-request (feature-name params)
  "Handle GET request for feature content.
   
   This handler:
   1. Extracts or creates session ID from cookies
   2. Detects language from session or Accept-Language header
   3. Generates HTML form for the requested feature
   4. Returns HTML response with session cookie
   
   Parameters:
   -----------
   - feature-name: Name of the feature (string)
   - params: Alist of request parameters from Ningle
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)
   
   Response Format:
   ----------------
   Success (200): HTML form for the feature
   Error (404): Feature not found
   Error (500): Server error"
  (wrap-handler
   (lambda ()
     ;; Extract session ID
     (let* ((session-id (extract-session-id-from-params params))
            (existing-session (and session-id (get-session session-id))))
       
       ;; Detect language from request (before creating session)
       (let ((language (if existing-session
                          ;; Use existing session language
                          (get-session-language session-id)
                          ;; Detect from request for new session
                          (detect-language-from-request params))))
         
         ;; Create or get session
         (let* ((session (get-or-create-session session-id))
                (new-session-id (session-id session)))
           
           ;; If this is a new session, set the detected language
           (unless existing-session
             (set-session-language new-session-id language))
           
           ;; Update session language if explicitly provided
           (let ((explicit-lang (get-param params "language")))
             (when (and explicit-lang (validate-language explicit-lang))
               (set-session-language new-session-id explicit-lang)
               (setf language explicit-lang)))
           
           ;; Find feature generator
           (let ((generator-entry (assoc feature-name *feature-generators* 
                                        :test #'string=)))
             (if generator-entry
                 (let* ((generator-fn (cdr generator-entry))
                        ;; For character-chat, pass history and character
                        (html-content (cond
                                       ((string= feature-name "character-chat")
                                        (let* ((history (get-conversation-history 
                                                        new-session-id 
                                                        "character-chat"))
                                               (character (or (get-param params "character") 
                                                            "dracula")))
                                          (funcall generator-fn language history character t)))
                                       ;; For trivia-bot, pass history
                                       ((string= feature-name "trivia-bot")
                                        (let ((history (get-conversation-history 
                                                       new-session-id 
                                                       "trivia-bot")))
                                          (funcall generator-fn language history :streaming-p t)))
                                       ;; For other features, just pass language with streaming enabled
                                       (t (funcall generator-fn language :streaming-p t)))))
                   ;; Return HTML with session cookie
                   `(200 (:content-type "text/html; charset=utf-8"
                         :set-cookie ,(create-session-cookie new-session-id))
                     (,html-content)))
                 ;; Feature not found
                 `(404 (:content-type "text/html; charset=utf-8")
                   (,(generate-alert
                      :type :error
                      :message (if (string= language "ja")
                                  (format nil "機能が見つかりません: ~A" feature-name)
                                  (format nil "Feature not found: ~A" feature-name)))))))))))
   params
   :streaming-p nil))

;;; ----------------------------------------------------------------------------
;;; Language Switching Handler
;;; ----------------------------------------------------------------------------

(defun handle-set-language (params)
  "Handle POST /api/set-language request.
   
   This handler:
   1. Extracts or creates session ID from cookies
   2. Validates the requested language parameter
   3. Updates session language preference
   4. Returns updated feature content in the new language
   
   Parameters:
   -----------
   - params: Alist of request parameters from Ningle
   
   Expected Parameters:
   --------------------
   - language: Target language code ('en' or 'ja')
   - feature: Optional feature name to reload (defaults to 'monster-diagnostic')
   
   Returns:
   --------
   - HTTP response list: (status-code headers body)
   
   Response Format:
   ----------------
   Success (200): HTML content for the requested feature in new language
   Error (400): Invalid language parameter
   Error (404): Feature not found
   Error (500): Server error
   
   Examples:
   ---------
   POST /api/set-language
   Body: language=en&feature=character-chat
   => Returns character-chat form in English"
  (wrap-handler
   (lambda ()
     ;; Extract and validate language parameter
     (let ((language (get-param params "language")))
       (if (not (and language (validate-language language)))
           ;; Return validation error
           (build-validation-error-response
            (if (string= language "ja")
                "無効な言語パラメータです"
                "Invalid language parameter")
            (or language "en")
            :streaming-p nil)
           ;; Process language change
           (let* ((session-id (extract-session-id-from-params params))
                  (session (get-or-create-session session-id))
                  (new-session-id (session-id session)))
             
             ;; Update session language preference
             (set-session-language new-session-id language)
             
             ;; Get feature to reload (default to monster-diagnostic)
             (let ((feature (or (get-param params "feature") "monster-diagnostic")))
               
               ;; Find feature generator
               (let ((generator-entry (assoc feature *feature-generators* 
                                            :test #'string=)))
                 (if generator-entry
                     (let* ((generator-fn (cdr generator-entry))
                            ;; Generate HTML content for the feature
                            (html-content (cond
                                           ;; For character-chat, pass history and character
                                           ((string= feature "character-chat")
                                            (let* ((history (get-conversation-history 
                                                            new-session-id 
                                                            "character-chat"))
                                                   (character (or (get-param params "character") 
                                                                "dracula")))
                                              (funcall generator-fn language history character t)))
                                           ;; For trivia-bot, pass history
                                           ((string= feature "trivia-bot")
                                            (let ((history (get-conversation-history 
                                                           new-session-id 
                                                           "trivia-bot")))
                                              (funcall generator-fn language history :streaming-p t)))
                                           ;; For other features, just pass language with streaming enabled
                                           (t (funcall generator-fn language :streaming-p t)))))
                       ;; Return HTML with session cookie
                       `(200 (:content-type "text/html; charset=utf-8"
                             :set-cookie ,(create-session-cookie new-session-id))
                         (,html-content)))
                     ;; Feature not found
                     `(404 (:content-type "text/html; charset=utf-8")
                       (,(generate-alert
                          :type :error
                          :message (if (string= language "ja")
                                      (format nil "機能が見つかりません: ~A" feature)
                                      (format nil "Feature not found: ~A" feature))))))))))))
   params
   :streaming-p nil))

;;; ----------------------------------------------------------------------------
;;; Individual Feature Handlers
;;; ----------------------------------------------------------------------------

(defun handle-get-monster-diagnostic (params)
  "Handle GET /api/features/monster-diagnostic"
  (handle-feature-content-request "monster-diagnostic" params))

(defun handle-get-story-generator (params)
  "Handle GET /api/features/story-generator"
  (handle-feature-content-request "story-generator" params))

(defun handle-get-character-chat (params)
  "Handle GET /api/features/character-chat"
  (handle-feature-content-request "character-chat" params))

(defun handle-get-trivia-bot (params)
  "Handle GET /api/features/trivia-bot"
  (handle-feature-content-request "trivia-bot" params))

(defun handle-get-spell-generator (params)
  "Handle GET /api/features/spell-generator"
  (handle-feature-content-request "spell-generator" params))
