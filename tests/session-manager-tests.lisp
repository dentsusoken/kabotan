;;;; Session Manager Tests

(in-package :kabotan.tests)

;;; Test Suite

(def-suite session-manager-suite
  :in kabotan-suite
  :description "Session management system tests")

(in-suite session-manager-suite)

;;; Session ID Generation Tests

(test generate-session-id-format
  "Test that generate-session-id produces valid hex string"
  (let ((session-id (kabotan::generate-session-id)))
    (is (stringp session-id))
    (is (= 64 (length session-id)))  ; 32 bytes = 64 hex chars
    (is (every (lambda (c) (find c "0123456789abcdef" :test #'char-equal)) session-id))))

(test generate-session-id-uniqueness
  "Test that generate-session-id produces unique IDs"
  (let ((id1 (kabotan::generate-session-id))
        (id2 (kabotan::generate-session-id))
        (id3 (kabotan::generate-session-id)))
    (is (not (string= id1 id2)))
    (is (not (string= id2 id3)))
    (is (not (string= id1 id3)))))

;;; Session Creation and Retrieval Tests

(test create-session-basic
  "Test that create-session creates a new session"
  (kabotan::initialize-session-manager)
  (let ((session (kabotan::create-session)))
    (is (kabotan::session-p session))
    (is (stringp (kabotan::session-id session)))
    (is (string= "ja" (kabotan::session-language session)))
    (is (numberp (kabotan::session-created-at session)))
    (is (numberp (kabotan::session-last-accessed session)))
    (is (hash-table-p (kabotan::session-conversations session)))))

(test get-session-existing
  "Test that get-session retrieves existing session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session))
         (retrieved (kabotan::get-session session-id)))
    (is (kabotan::session-p retrieved))
    (is (string= session-id (kabotan::session-id retrieved)))))

(test get-session-nonexistent
  "Test that get-session returns nil for nonexistent session"
  (kabotan::initialize-session-manager)
  (let ((retrieved (kabotan::get-session "nonexistent-id")))
    (is (null retrieved))))

(test get-or-create-session-existing
  "Test that get-or-create-session returns existing session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session))
         (retrieved (kabotan::get-or-create-session session-id)))
    (is (string= session-id (kabotan::session-id retrieved)))))

(test get-or-create-session-new
  "Test that get-or-create-session creates new session when not found"
  (kabotan::initialize-session-manager)
  (let ((session (kabotan::get-or-create-session "nonexistent-id")))
    (is (kabotan::session-p session))
    (is (stringp (kabotan::session-id session)))))

;;; Cookie Handling Tests

(test extract-session-id-from-cookie-single
  "Test extracting session ID from single cookie"
  (let* ((session-id "abc123")
         (cookie-header (format nil "session_id=~A" session-id))
         (extracted (kabotan::extract-session-id-from-cookie cookie-header)))
    (is (string= session-id extracted))))

(test extract-session-id-from-cookie-multiple
  "Test extracting session ID from multiple cookies"
  (let* ((session-id "abc123")
         (cookie-header (format nil "other=value; session_id=~A; another=test" session-id))
         (extracted (kabotan::extract-session-id-from-cookie cookie-header)))
    (is (string= session-id extracted))))

(test extract-session-id-from-cookie-missing
  "Test extracting session ID when not present"
  (let* ((cookie-header "other=value; another=test")
         (extracted (kabotan::extract-session-id-from-cookie cookie-header)))
    (is (null extracted))))

(test extract-session-id-from-cookie-nil
  "Test extracting session ID from nil cookie header"
  (let ((extracted (kabotan::extract-session-id-from-cookie nil)))
    (is (null extracted))))

(test create-session-cookie-format
  "Test that create-session-cookie produces correct format"
  (let* ((session-id "abc123")
         (cookie (kabotan::create-session-cookie session-id)))
    (is (stringp cookie))
    (is (search "session_id=abc123" cookie))
    (is (search "Path=/" cookie))
    (is (search "HttpOnly" cookie))
    (is (search "SameSite=Lax" cookie))
    (is (search "Max-Age=" cookie))))

;;; Language Preference Tests

(test get-session-language-existing
  "Test getting language from existing session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (setf (kabotan::session-language session) "en")
    (is (string= "en" (kabotan::get-session-language session-id)))))

(test get-session-language-default
  "Test getting language defaults to 'ja' for nonexistent session"
  (kabotan::initialize-session-manager)
  (is (string= "ja" (kabotan::get-session-language "nonexistent-id"))))

(test set-session-language-existing
  "Test setting language on existing session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::set-session-language session-id "en")
    (is (string= "en" (kabotan::session-language session)))))

(test set-session-language-new
  "Test setting language creates new session if not exists"
  (kabotan::initialize-session-manager)
  (let ((session (kabotan::set-session-language "new-id" "en")))
    (is (kabotan::session-p session))
    (is (string= "en" (kabotan::session-language session)))))

;;; Conversation History Tests

(test get-conversation-history-empty
  "Test getting conversation history for new session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session))
         (history (kabotan::get-conversation-history session-id "character-chat")))
    (is (null history))))

(test add-to-conversation-history-single
  "Test adding single message to conversation history"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::add-to-conversation-history session-id "character-chat" "user" "Hello")
    (let ((history (kabotan::get-conversation-history session-id "character-chat")))
      (is (= 1 (length history)))
      (is (string= "user" (kabotan::conversation-message-role (first history))))
      (is (string= "Hello" (kabotan::conversation-message-content (first history)))))))

(test add-to-conversation-history-separate-features
  "Test that conversation histories are separate per feature"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::add-to-conversation-history session-id "character-chat" "user" "Chat message")
    (kabotan::add-to-conversation-history session-id "trivia-bot" "user" "Trivia question")
    (let ((chat-history (kabotan::get-conversation-history session-id "character-chat"))
          (trivia-history (kabotan::get-conversation-history session-id "trivia-bot")))
      (is (= 1 (length chat-history)))
      (is (= 1 (length trivia-history)))
      (is (string= "Chat message" (kabotan::conversation-message-content (first chat-history))))
      (is (string= "Trivia question" (kabotan::conversation-message-content (first trivia-history)))))))

(test clear-conversation-history-existing
  "Test clearing conversation history for a feature"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::add-to-conversation-history session-id "character-chat" "user" "Hello")
    (kabotan::add-to-conversation-history session-id "character-chat" "assistant" "Hi")
    (kabotan::clear-conversation-history session-id "character-chat")
    (let ((history (kabotan::get-conversation-history session-id "character-chat")))
      (is (null history)))))

;;; Session Cleanup Tests

(test cleanup-old-sessions-none-expired
  "Test that cleanup doesn't remove recent sessions"
  (kabotan::initialize-session-manager)
  (kabotan::create-session)
  (kabotan::create-session)
  (let ((initial-count (kabotan::get-session-count)))
    (is (= 2 initial-count))
    (let ((removed (kabotan::cleanup-old-sessions)))
      (is (= 0 removed))
      (is (= 2 (kabotan::get-session-count))))))

(test get-session-count-empty
  "Test getting session count when empty"
  (kabotan::initialize-session-manager)
  (is (= 0 (kabotan::get-session-count))))

(test get-session-count-multiple
  "Test getting session count with multiple sessions"
  (kabotan::initialize-session-manager)
  (kabotan::create-session)
  (kabotan::create-session)
  (kabotan::create-session)
  (is (= 3 (kabotan::get-session-count))))

(test get-session-info-existing
  "Test getting session info for existing session"
  (kabotan::initialize-session-manager)
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::set-session-language session-id "en")
    (kabotan::add-to-conversation-history session-id "character-chat" "user" "Hello")
    (let ((info (kabotan::get-session-info session-id)))
      (is (listp info))
      (is (string= session-id (getf info :id)))
      (is (string= "en" (getf info :language)))
      (is (numberp (getf info :created-at)))
      (is (numberp (getf info :last-accessed)))
      (is (= 1 (getf info :conversation-count))))))

(test get-session-info-nonexistent
  "Test getting session info for nonexistent session"
  (kabotan::initialize-session-manager)
  (let ((info (kabotan::get-session-info "nonexistent-id")))
    (is (null info))))
