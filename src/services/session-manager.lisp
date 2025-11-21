;;;; Session Management System
;;;; Provides session storage, language preferences, and conversation history

(in-package :kabotan)

;;; Session Data Structures

(defstruct session
  "Session data structure containing user state"
  (id nil :type (or null string))
  (language "ja" :type string)
  (created-at (get-universal-time) :type integer)
  (last-accessed (get-universal-time) :type integer)
  (conversations (make-hash-table :test 'equal) :type hash-table))

(defstruct conversation-message
  "Individual message in a conversation"
  (role nil :type string)           ; "user" or "assistant"
  (content nil :type string)
  (timestamp (get-universal-time) :type integer))

;;; Global Session Storage

(defparameter *sessions* (make-hash-table :test 'equal)
  "Global hash table storing all active sessions")

(defparameter *session-timeout* (* 24 60 60)
  "Session timeout in seconds (24 hours)")

(defparameter *session-cleanup-interval* (* 60 60)
  "Interval between session cleanup runs in seconds (1 hour)")

(defparameter *last-cleanup-time* (get-universal-time)
  "Timestamp of last session cleanup")

;;; Session ID Generation

(defun generate-session-id ()
  "Generate a cryptographically secure random session ID"
  (let ((random-bytes (make-array 32 :element-type '(unsigned-byte 8))))
    ;; Fill with random bytes
    (dotimes (i 32)
      (setf (aref random-bytes i) (random 256)))
    ;; Convert to hex string
    (format nil "~{~2,'0x~}" (coerce random-bytes 'list))))

;;; Session Creation and Retrieval

(defun create-session ()
  "Create a new session and return the session object"
  (let* ((session-id (generate-session-id))
         (new-session (make-session :id session-id)))
    (setf (gethash session-id *sessions*) new-session)
    (log-session-event "CREATE" session-id)
    new-session))

(defun get-session (session-id)
  "Retrieve session by ID, return NIL if not found or expired"
  (when session-id
    (let ((session (gethash session-id *sessions*)))
      (when session
        ;; Update last accessed time
        (setf (session-last-accessed session) (get-universal-time))
        session))))

(defun get-or-create-session (session-id)
  "Get existing session or create new one if not found"
  (or (get-session session-id)
      (create-session)))

;;; Cookie Handling

(defun extract-session-id-from-cookie (cookie-header)
  "Extract session ID from Cookie header string"
  (when cookie-header
    (let ((cookies (cl-ppcre:split ";\\s*" cookie-header)))
      (dolist (cookie cookies)
        (let ((parts (cl-ppcre:split "=" cookie)))
          (when (and (= (length parts) 2)
                     (string= (string-trim '(#\Space) (first parts)) "session_id"))
            (return-from extract-session-id-from-cookie 
              (string-trim '(#\Space) (second parts)))))))))

(defun create-session-cookie (session-id)
  "Create Set-Cookie header value for session ID"
  (format nil "session_id=~A; Path=/; HttpOnly; SameSite=Lax; Max-Age=~D"
          session-id
          *session-timeout*))

;;; Language Preference Management

(defun get-session-language (session-id)
  "Retrieve language preference from session, default to 'ja'"
  (let ((session (get-session session-id)))
    (if session
        (session-language session)
        "ja")))

(defun set-session-language (session-id language)
  "Store language preference in session"
  (let ((session (get-or-create-session session-id)))
    (setf (session-language session) language)
    (log-session-event "SET-LANGUAGE" session-id language)
    session))

;;; Conversation History Management

(defun get-conversation-history (session-id feature)
  "Retrieve conversation history for a specific feature"
  (let ((session (get-session session-id)))
    (when session
      (gethash feature (session-conversations session)))))

(defun add-to-conversation-history (session-id feature role content)
  "Add a message to conversation history for a specific feature"
  (let ((session (get-or-create-session session-id)))
    (let ((message (make-conversation-message
                    :role role
                    :content content
                    :timestamp (get-universal-time))))
      ;; Get or create conversation list for this feature
      (let ((history (gethash feature (session-conversations session))))
        (setf (gethash feature (session-conversations session))
              (append history (list message))))
      (log-session-event "ADD-MESSAGE" session-id 
                        (format nil "~A:~A" feature role))
      message)))

(defun clear-conversation-history (session-id feature)
  "Clear conversation history for a specific feature"
  (let ((session (get-session session-id)))
    (when session
      (remhash feature (session-conversations session))
      (log-session-event "CLEAR-HISTORY" session-id feature))))

(defun format-conversation-for-llm (session-id feature)
  "Format conversation history for LLM API (list of role/content pairs)"
  (let ((history (get-conversation-history session-id feature)))
    (when history
      (mapcar (lambda (msg)
                (list (cons "role" (conversation-message-role msg))
                      (cons "content" (conversation-message-content msg))))
              history))))

;;; Session Cleanup

(defun cleanup-old-sessions ()
  "Remove sessions older than timeout period"
  (let ((cutoff (- (get-universal-time) *session-timeout*))
        (removed-count 0))
    (maphash (lambda (id session)
               (when (< (session-last-accessed session) cutoff)
                 (remhash id *sessions*)
                 (incf removed-count)
                 (log-session-event "CLEANUP" id)))
             *sessions*)
    (setf *last-cleanup-time* (get-universal-time))
    (when (> removed-count 0)
      (format t "[SESSION-CLEANUP] Removed ~D expired sessions~%" removed-count))
    removed-count))

(defun maybe-cleanup-sessions ()
  "Run session cleanup if enough time has passed since last cleanup"
  (when (> (- (get-universal-time) *last-cleanup-time*)
           *session-cleanup-interval*)
    (cleanup-old-sessions)))

;;; Session Statistics

(defun get-session-count ()
  "Return the number of active sessions"
  (hash-table-count *sessions*))

(defun get-session-info (session-id)
  "Get information about a session for debugging"
  (let ((session (get-session session-id)))
    (when session
      (list :id (session-id session)
            :language (session-language session)
            :created-at (session-created-at session)
            :last-accessed (session-last-accessed session)
            :conversation-count (hash-table-count (session-conversations session))))))

;;; Logging Utilities

(defun log-session-event (event session-id &optional details)
  "Log session-related events"
  (if details
      (format t "[SESSION] ~A: ~A (~A)~%" event session-id details)
      (format t "[SESSION] ~A: ~A~%" event session-id)))

;;; Initialization

(defun initialize-session-manager ()
  "Initialize session manager (called on application startup)"
  (setf *sessions* (make-hash-table :test 'equal))
  (setf *last-cleanup-time* (get-universal-time))
  (format t "[SESSION] Session manager initialized~%"))
