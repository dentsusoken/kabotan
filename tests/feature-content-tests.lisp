(in-package :kabotan.tests)

;;; ============================================================================
;;; Feature Content Handler Tests
;;; ============================================================================

(def-suite feature-content-suite
  :in kabotan.tests::kabotan-suite
  :description "Tests for feature content endpoint infrastructure")

(in-suite feature-content-suite)

;;; ----------------------------------------------------------------------------
;;; Session ID Extraction Tests
;;; ----------------------------------------------------------------------------

(test extract-session-id-from-params-with-cookie
  "Test extracting session ID from cookie header"
  (let ((params '((:cookie . "session_id=test123; other=value"))))
    (is (string= "test123" (kabotan::extract-session-id-from-params params)))))

(test extract-session-id-from-params-without-cookie
  "Test extracting session ID when no cookie present"
  (let ((params '()))
    (is (null (kabotan::extract-session-id-from-params params)))))

(test extract-session-id-from-params-with-invalid-cookie
  "Test extracting session ID from invalid cookie format"
  (let ((params '((:cookie . "other=value; foo=bar"))))
    (is (null (kabotan::extract-session-id-from-params params)))))

;;; ----------------------------------------------------------------------------
;;; Accept-Language Header Tests
;;; ----------------------------------------------------------------------------

(test extract-language-from-accept-header-japanese
  "Test extracting Japanese from Accept-Language header"
  (let ((params '((:accept-language . "ja,en-US;q=0.9,en;q=0.8"))))
    (is (string= "ja" (kabotan::extract-language-from-accept-header params)))))

(test extract-language-from-accept-header-english
  "Test extracting English from Accept-Language header"
  (let ((params '((:accept-language . "en-US,en;q=0.9,ja;q=0.8"))))
    (is (string= "en" (kabotan::extract-language-from-accept-header params)))))

(test extract-language-from-accept-header-no-header
  "Test extracting language when no Accept-Language header present"
  (let ((params '()))
    (is (null (kabotan::extract-language-from-accept-header params)))))

(test extract-language-from-accept-header-unsupported
  "Test extracting language when only unsupported languages present"
  (let ((params '((:accept-language . "fr,de;q=0.9"))))
    (is (null (kabotan::extract-language-from-accept-header params)))))

;;; ----------------------------------------------------------------------------
;;; Language Detection Tests
;;; ----------------------------------------------------------------------------

(test detect-language-explicit-parameter
  "Test language detection with explicit parameter"
  (let ((params '(("language" . "en")
                  (:accept-language . "ja"))))
    (is (string= "en" (kabotan::detect-language-from-request params)))))

(test detect-language-from-accept-header
  "Test language detection from Accept-Language header"
  (let ((params '((:accept-language . "en-US,en;q=0.9"))))
    (is (string= "en" (kabotan::detect-language-from-request params)))))

(test detect-language-default
  "Test language detection defaults to Japanese"
  (let ((params '()))
    (is (string= "ja" (kabotan::detect-language-from-request params)))))

(test detect-language-from-session
  "Test language detection from session"
  ;; Create a session with English preference
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::set-session-language session-id "en")
    (let ((params `((:cookie . ,(format nil "session_id=~A" session-id)))))
      (is (string= "en" (kabotan::detect-language-from-request params))))))

;;; ----------------------------------------------------------------------------
;;; Feature Content Handler Tests
;;; ----------------------------------------------------------------------------

(test handle-feature-content-monster-diagnostic
  "Test handling monster diagnostic feature content request"
  (let ((params '()))
    (let ((response (kabotan::handle-get-monster-diagnostic params)))
      (is (= 200 (first response)))
      (is (search "text/html" (getf (second response) :content-type)))
      (is (not (null (getf (second response) :set-cookie))))
      (is (search "モンスター診断" (first (third response)))))))

(test handle-feature-content-story-generator
  "Test handling story generator feature content request"
  (let ((params '()))
    (let ((response (kabotan::handle-get-story-generator params)))
      (is (= 200 (first response)))
      (is (search "text/html" (getf (second response) :content-type)))
      (is (search "ストーリー生成" (first (third response)))))))

(test handle-feature-content-character-chat
  "Test handling character chat feature content request"
  (let ((params '()))
    (let ((response (kabotan::handle-get-character-chat params)))
      (is (= 200 (first response)))
      (is (search "text/html" (getf (second response) :content-type)))
      (is (search "キャラクターチャット" (first (third response)))))))

(test handle-feature-content-trivia-bot
  "Test handling trivia bot feature content request"
  (let ((params '()))
    (let ((response (kabotan::handle-get-trivia-bot params)))
      (is (= 200 (first response)))
      (is (search "text/html" (getf (second response) :content-type)))
      (is (search "ハロウィントリビア" (first (third response)))))))

(test handle-feature-content-spell-generator
  "Test handling spell generator feature content request"
  (let ((params '()))
    (let ((response (kabotan::handle-get-spell-generator params)))
      (is (= 200 (first response)))
      (is (search "text/html" (getf (second response) :content-type)))
      (is (search "呪文生成" (first (third response)))))))

(test handle-feature-content-with-language-parameter
  "Test handling feature content request with explicit language parameter"
  (let ((params '(("language" . "en"))))
    (let ((response (kabotan::handle-get-spell-generator params)))
      (is (= 200 (first response)))
      (is (search "Spell Generator" (first (third response)))))))

(test handle-feature-content-with-session
  "Test handling feature content request with existing session"
  ;; Create a session with English preference
  (let* ((session (kabotan::create-session))
         (session-id (kabotan::session-id session)))
    (kabotan::set-session-language session-id "en")
    (let ((params `((:cookie . ,(format nil "session_id=~A" session-id)))))
      (let ((response (kabotan::handle-get-spell-generator params)))
        (is (= 200 (first response)))
        (is (search "Spell Generator" (first (third response))))))))

(test handle-feature-content-creates-session
  "Test that feature content request creates session if none exists"
  (let ((params '()))
    (let ((response (kabotan::handle-get-spell-generator params)))
      (is (= 200 (first response)))
      (let ((set-cookie (getf (second response) :set-cookie)))
        (is (not (null set-cookie)))
        (is (search "session_id=" set-cookie))))))

(test handle-feature-content-invalid-feature
  "Test handling request for non-existent feature"
  (let ((params '()))
    (let ((response (kabotan::handle-feature-content-request "invalid-feature" params)))
      (is (= 404 (first response)))
      (is (search "機能が見つかりません" (first (third response)))))))
