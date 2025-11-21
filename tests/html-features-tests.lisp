(in-package :kabotan.tests)

;;; HTML Features and Chat Components Tests

(def-suite html-features-suite
  :in kabotan-suite
  :description "HTML feature-specific forms and chat components tests")

(in-suite html-features-suite)

;;; Feature-Specific Form Tests

(test generate-spell-generator-form-japanese
  "Test that generate-spell-generator-form creates form in Japanese"
  (let ((result (kabotan::generate-spell-generator-form "ja")))
    (is (search "呪文生成" result))
    (is (search "あなたのための魔法の言葉を生成します" result))
    (is (search "新しい呪文を生成" result))
    (is (search "hx-post=\"/api/spell-generator\"" result))
    (is (search "hx-target=\"#spell-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#spell-loading\"" result))
    (is (search "name=\"language\" value=\"ja\"" result))
    (is (search "id=\"spell-result-container\"" result))
    (is (search "id=\"spell-loading\"" result))))

(test generate-spell-generator-form-english
  "Test that generate-spell-generator-form creates form in English"
  (let ((result (kabotan::generate-spell-generator-form "en")))
    (is (search "Spell Generator" result))
    (is (search "Generate a magical phrase for you" result))
    (is (search "Generate New Spell" result))
    (is (search "hx-post=\"/api/spell-generator\"" result))
    (is (search "hx-target=\"#spell-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#spell-loading\"" result))
    (is (search "name=\"language\" value=\"en\"" result))
    (is (search "id=\"spell-result-container\"" result))
    (is (search "id=\"spell-loading\"" result))))

(test generate-monster-diagnostic-form-japanese
  "Test that generate-monster-diagnostic-form creates form in Japanese"
  (let ((result (kabotan::generate-monster-diagnostic-form "ja")))
    (is (search "モンスター診断" result))
    (is (search "あなたの性格に合ったハロウィンモンスターを診断します" result))
    (is (search "好きな食べ物" result))
    (is (search "睡眠スケジュール" result))
    (is (search "趣味" result))
    (is (search "恐れているもの" result))
    (is (search "診断する" result))
    (is (search "hx-post=\"/api/monster-diagnostic\"" result))
    (is (search "hx-target=\"#monster-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#monster-loading\"" result))
    (is (search "name=\"language\" value=\"ja\"" result))
    (is (search "name=\"favorite_food\"" result))
    (is (search "name=\"sleep_schedule\"" result))
    (is (search "name=\"hobby\"" result))
    (is (search "name=\"fear\"" result))
    (is (search "required" result))
    (is (search "id=\"monster-result-container\"" result))
    (is (search "id=\"monster-loading\"" result))))

(test generate-monster-diagnostic-form-english
  "Test that generate-monster-diagnostic-form creates form in English"
  (let ((result (kabotan::generate-monster-diagnostic-form "en")))
    (is (search "Monster Diagnostic" result))
    (is (search "Discover which Halloween monster matches your personality" result))
    (is (search "Favorite Food" result))
    (is (search "Sleep Schedule" result))
    (is (search "Hobby" result))
    (is (search "What You Fear" result))
    (is (search "Diagnose" result))
    (is (search "hx-post=\"/api/monster-diagnostic\"" result))
    (is (search "hx-target=\"#monster-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#monster-loading\"" result))
    (is (search "name=\"language\" value=\"en\"" result))
    (is (search "name=\"favorite_food\"" result))
    (is (search "name=\"sleep_schedule\"" result))
    (is (search "name=\"hobby\"" result))
    (is (search "name=\"fear\"" result))
    (is (search "required" result))
    (is (search "id=\"monster-result-container\"" result))
    (is (search "id=\"monster-loading\"" result))))

(test generate-story-generator-form-japanese
  "Test that generate-story-generator-form creates form in Japanese"
  (let ((result (kabotan::generate-story-generator-form "ja")))
    (is (search "ハロウィンストーリー生成" result))
    (is (search "あなただけのハロウィンストーリーを作成します" result))
    (is (search "名前" result))
    (is (search "テーマ" result))
    (is (search "スタイル" result))
    (is (search "ゴシック" result))
    (is (search "パロディ" result))
    (is (search "クラシック" result))
    (is (search "生成する" result))
    (is (search "hx-post=\"/api/story-generator\"" result))
    (is (search "hx-target=\"#story-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#story-loading\"" result))
    (is (search "name=\"language\" value=\"ja\"" result))
    (is (search "name=\"name\"" result))
    (is (search "name=\"theme\"" result))
    (is (search "name=\"style\"" result))
    (is (search "value=\"gothic\"" result))
    (is (search "value=\"parody\"" result))
    (is (search "value=\"classic\"" result))
    (is (search "checked" result))
    (is (search "required" result))
    (is (search "id=\"story-result-container\"" result))
    (is (search "id=\"story-loading\"" result))))

(test generate-story-generator-form-english
  "Test that generate-story-generator-form creates form in English"
  (let ((result (kabotan::generate-story-generator-form "en")))
    (is (search "Halloween Story Generator" result))
    (is (search "Create your own Halloween story" result))
    (is (search "Name" result))
    (is (search "Theme" result))
    (is (search "Style" result))
    (is (search "Gothic" result))
    (is (search "Parody" result))
    (is (search "Classic" result))
    (is (search "Generate" result))
    (is (search "hx-post=\"/api/story-generator\"" result))
    (is (search "hx-target=\"#story-result-container\"" result))
    (is (search "hx-swap=\"innerHTML\"" result))
    (is (search "hx-indicator=\"#story-loading\"" result))
    (is (search "name=\"language\" value=\"en\"" result))
    (is (search "name=\"name\"" result))
    (is (search "name=\"theme\"" result))
    (is (search "name=\"style\"" result))
    (is (search "value=\"gothic\"" result))
    (is (search "value=\"parody\"" result))
    (is (search "value=\"classic\"" result))
    (is (search "checked" result))
    (is (search "required" result))
    (is (search "id=\"story-result-container\"" result))
    (is (search "id=\"story-loading\"" result))))

;;; Character Chat Tests

(test generate-chat-message-user
  "Test that generate-chat-message creates user message"
  (let ((result (kabotan::generate-chat-message "user" "Hello!" "dracula" "en")))
    (is (search "chat chat-end" result))
    (is (search "chat-bubble-primary" result))
    (is (search "Hello!" result))
    (is (search "Message" result))))

(test generate-chat-message-assistant-dracula
  "Test that generate-chat-message creates assistant message for Dracula"
  (let ((result (kabotan::generate-chat-message "assistant" "Good evening!" "dracula" "en")))
    (is (search "chat chat-start" result))
    (is (search "chat-bubble-secondary" result))
    (is (search "Good evening!" result))
    (is (search "Dracula" result))))

(test generate-chat-message-assistant-witch
  "Test that generate-chat-message creates assistant message for Witch"
  (let ((result (kabotan::generate-chat-message "assistant" "Hehe!" "witch" "ja")))
    (is (search "chat chat-start" result))
    (is (search "chat-bubble-secondary" result))
    (is (search "Hehe!" result))
    (is (search "魔女" result))))

(test generate-chat-message-assistant-jack
  "Test that generate-chat-message creates assistant message for Jack"
  (let ((result (kabotan::generate-chat-message "assistant" "Boo!" "jack" "en")))
    (is (search "chat chat-start" result))
    (is (search "chat-bubble-secondary" result))
    (is (search "Boo!" result))
    (is (search "Jack-o&#39;-Lantern" result))))

(test generate-chat-message-escapes-html
  "Test that generate-chat-message escapes HTML in content"
  (let ((result (kabotan::generate-chat-message "user" "<script>alert('xss')</script>" "dracula" "en")))
    (is (search "&lt;script&gt;" result))
    (is (not (search "<script>" result)))))

(test generate-character-chat-form-japanese-no-history
  "Test that generate-character-chat-form creates form in Japanese without history"
  (let ((result (kabotan::generate-character-chat-form "ja")))
    (is (search "キャラクターチャット" result))
    (is (search "ハロウィンキャラクターと会話しましょう" result))
    (is (search "キャラクター" result))
    (is (search "ドラキュラ" result))
    (is (search "魔女" result))
    (is (search "ジャック・オー・ランタン" result))
    (is (search "メッセージ" result))
    (is (search "送信" result))
    (is (search "hx-post=\"/api/character-chat\"" result))
    (is (search "hx-target=\"#chat-history-container\"" result))
    (is (search "hx-swap=\"beforeend\"" result))
    (is (search "hx-indicator=\"#chat-loading\"" result))
    (is (search "name=\"language\" value=\"ja\"" result))
    (is (search "name=\"character\"" result))
    (is (search "value=\"dracula\"" result))
    (is (search "name=\"message\"" result))
    (is (search "required" result))
    (is (search "id=\"chat-history-container\"" result))
    (is (search "id=\"chat-loading\"" result))))

(test generate-character-chat-form-english-no-history
  "Test that generate-character-chat-form creates form in English without history"
  (let ((result (kabotan::generate-character-chat-form "en")))
    (is (search "Character Chat" result))
    (is (search "Chat with Halloween characters" result))
    (is (search "Character" result))
    (is (search "Dracula" result))
    (is (search "Witch" result))
    (is (search "Jack-o&#39;-Lantern" result))
    (is (search "Message" result))
    (is (search "Send" result))
    (is (search "hx-post=\"/api/character-chat\"" result))
    (is (search "hx-target=\"#chat-history-container\"" result))
    (is (search "hx-swap=\"beforeend\"" result))
    (is (search "hx-indicator=\"#chat-loading\"" result))
    (is (search "name=\"language\" value=\"en\"" result))
    (is (search "name=\"character\"" result))
    (is (search "value=\"dracula\"" result))
    (is (search "name=\"message\"" result))
    (is (search "required" result))
    (is (search "id=\"chat-history-container\"" result))
    (is (search "id=\"chat-loading\"" result))))

(test generate-character-chat-form-with-history
  "Test that generate-character-chat-form includes conversation history"
  (let* ((msg1 (kabotan::make-conversation-message 
                :role "user" 
                :content "Hello!"
                :timestamp (get-universal-time)))
         (msg2 (kabotan::make-conversation-message 
                :role "assistant" 
                :content "Good evening!"
                :timestamp (get-universal-time)))
         (history (list msg1 msg2))
         (result (kabotan::generate-character-chat-form "en" history "dracula")))
    (is (search "Character Chat" result))
    (is (search "Hello!" result))
    (is (search "Good evening!" result))
    (is (search "chat chat-end" result))
    (is (search "chat chat-start" result))
    (is (search "Dracula" result))))

(test generate-character-chat-form-with-selected-character
  "Test that generate-character-chat-form uses selected character"
  (let ((result (kabotan::generate-character-chat-form "en" nil "witch")))
    (is (search "name=\"character\"" result))
    (is (search "value=\"witch\"" result))
    (is (search "checked" result))))

(test generate-character-chat-form-streaming-version
  "Test that generate-character-chat-form creates streaming version with HTMX SSE attributes"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" t)))
    ;; Should have history container
    (is (search "id=\"chat-history-container\"" result))
    ;; Should have HTMX SSE extension
    (is (search "hx-ext=\"sse\"" result))
    ;; Should have sse-swap attribute
    (is (search "sse-swap=\"message\"" result))
    ;; Should use GET method for streaming
    (is (search "hx-get=\"/api/character-chat-stream\"" result))
    ;; Should have character display name in header
    (is (search "Dracula" result))))

(test generate-character-chat-form-non-streaming-version
  "Test that generate-character-chat-form creates non-streaming version without SSE attributes"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" nil)))
    ;; Should have history container (same ID for both versions)
    (is (search "id=\"chat-history-container\"" result))
    ;; Should NOT have HTMX SSE extension on container
    (is (not (search "hx-ext=\"sse\"" result)))
    ;; Should use POST method for non-streaming
    (is (search "hx-post=\"/api/character-chat\"" result))))

(test get-character-display-name-english
  "Test that get-character-display-name returns correct English names"
  (is (string= (kabotan::get-character-display-name "dracula" "en") "Dracula"))
  (is (string= (kabotan::get-character-display-name "witch" "en") "Witch"))
  (is (string= (kabotan::get-character-display-name "jack" "en") "Jack-o'-Lantern"))
  (is (string= (kabotan::get-character-display-name "unknown" "en") "unknown")))

(test get-character-display-name-japanese
  "Test that get-character-display-name returns correct Japanese names"
  (is (string= (kabotan::get-character-display-name "dracula" "ja") "ドラキュラ"))
  (is (string= (kabotan::get-character-display-name "witch" "ja") "魔女"))
  (is (string= (kabotan::get-character-display-name "jack" "ja") "ジャック・オー・ランタン"))
  (is (string= (kabotan::get-character-display-name "unknown" "ja") "unknown")))

;;; Trivia Bot Tests

(test generate-trivia-message-user
  "Test that generate-trivia-message creates user message"
  (let ((result (kabotan::generate-trivia-message "user" "What is Halloween?" "en")))
    (is (search "chat chat-end" result))
    (is (search "chat-bubble-primary" result))
    (is (search "What is Halloween?" result))
    (is (search "Question" result))))

(test generate-trivia-message-assistant
  "Test that generate-trivia-message creates assistant message"
  (let ((result (kabotan::generate-trivia-message "assistant" "Halloween is a celebration..." "en")))
    (is (search "chat chat-start" result))
    (is (search "chat-bubble-secondary" result))
    (is (search "Halloween is a celebration..." result))
    (is (search "Trivia Bot" result))))

(test generate-trivia-message-escapes-html
  "Test that generate-trivia-message escapes HTML in content"
  (let ((result (kabotan::generate-trivia-message "user" "<script>alert('xss')</script>" "en")))
    (is (search "&lt;script&gt;" result))
    (is (not (search "<script>" result)))))

(test generate-trivia-bot-form-japanese-no-history
  "Test that generate-trivia-bot-form creates form in Japanese without history"
  (let ((result (kabotan::generate-trivia-bot-form "ja")))
    (is (search "ハロウィントリビア" result))
    (is (search "ハロウィンの豆知識を学びましょう" result))
    (is (search "質問" result))
    (is (search "送信" result))
    (is (search "hx-post=\"/api/trivia-bot\"" result))
    (is (search "hx-target=\"#trivia-history-container\"" result))
    (is (search "hx-swap=\"beforeend\"" result))
    (is (search "hx-indicator=\"#trivia-loading\"" result))
    (is (search "name=\"language\" value=\"ja\"" result))
    (is (search "name=\"question\"" result))
    (is (search "required" result))
    (is (search "id=\"trivia-history-container\"" result))
    (is (search "id=\"trivia-loading\"" result))))

(test generate-trivia-bot-form-english-no-history
  "Test that generate-trivia-bot-form creates form in English without history"
  (let ((result (kabotan::generate-trivia-bot-form "en")))
    (is (search "Halloween Trivia" result))
    (is (search "Learn interesting facts about Halloween" result))
    (is (search "Question" result))
    (is (search "Send" result))
    (is (search "hx-post=\"/api/trivia-bot\"" result))
    (is (search "hx-target=\"#trivia-history-container\"" result))
    (is (search "hx-swap=\"beforeend\"" result))
    (is (search "hx-indicator=\"#trivia-loading\"" result))
    (is (search "name=\"language\" value=\"en\"" result))
    (is (search "name=\"question\"" result))
    (is (search "required" result))
    (is (search "id=\"trivia-history-container\"" result))
    (is (search "id=\"trivia-loading\"" result))))

(test generate-trivia-bot-form-with-history
  "Test that generate-trivia-bot-form includes conversation history"
  (let* ((msg1 (kabotan::make-conversation-message 
                :role "user" 
                :content "What is Halloween?"
                :timestamp (get-universal-time)))
         (msg2 (kabotan::make-conversation-message 
                :role "assistant" 
                :content "Halloween is a celebration observed on October 31st."
                :timestamp (get-universal-time)))
         (history (list msg1 msg2))
         (result (kabotan::generate-trivia-bot-form "en" history)))
    (is (search "Halloween Trivia" result))
    (is (search "What is Halloween?" result))
    (is (search "Halloween is a celebration observed on October 31st." result))
    (is (search "chat chat-end" result))
    (is (search "chat chat-start" result))
    (is (search "Trivia Bot" result))))

;;; Character Selector Placement Tests

(test character-selector-inside-form-streaming
  "Test that character selector appears inside form element in streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" t)))
    ;; Find the form opening tag
    (let ((form-start (search "<form" result)))
      (is (not (null form-start)) "Form element should exist")
      ;; Find the form closing tag
      (let ((form-end (search "</form>" result :start2 form-start)))
        (is (not (null form-end)) "Form closing tag should exist")
        ;; Find character selector (radio button with name="character")
        (let ((character-selector (search "name=\"character\"" result :start2 form-start)))
          (is (not (null character-selector)) "Character selector should exist")
          ;; Verify character selector is between form tags
          (is (and character-selector (< form-start character-selector form-end))
              "Character selector should be inside form element"))))))

(test character-selector-inside-form-non-streaming
  "Test that character selector appears inside form element in non-streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" nil)))
    ;; Find the form opening tag
    (let ((form-start (search "<form" result)))
      (is (not (null form-start)) "Form element should exist")
      ;; Find the form closing tag
      (let ((form-end (search "</form>" result :start2 form-start)))
        (is (not (null form-end)) "Form closing tag should exist")
        ;; Find character selector (radio button with name="character")
        (let ((character-selector (search "name=\"character\"" result :start2 form-start)))
          (is (not (null character-selector)) "Character selector should exist")
          ;; Verify character selector is between form tags
          (is (and character-selector (< form-start character-selector form-end))
              "Character selector should be inside form element"))))))

(test character-selector-after-language-input-streaming
  "Test that character selector appears after language input in streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" t)))
    ;; Find language input
    (let ((language-input (search "name=\"language\"" result)))
      (is (not (null language-input)) "Language input should exist")
      ;; Find character selector
      (let ((character-selector (search "name=\"character\"" result)))
        (is (not (null character-selector)) "Character selector should exist")
        ;; Verify character selector comes after language input
        (is (and language-input character-selector (< language-input character-selector))
            "Character selector should appear after language input")))))

(test character-selector-after-language-input-non-streaming
  "Test that character selector appears after language input in non-streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" nil)))
    ;; Find language input
    (let ((language-input (search "name=\"language\"" result)))
      (is (not (null language-input)) "Language input should exist")
      ;; Find character selector
      (let ((character-selector (search "name=\"character\"" result)))
        (is (not (null character-selector)) "Character selector should exist")
        ;; Verify character selector comes after language input
        (is (and language-input character-selector (< language-input character-selector))
            "Character selector should appear after language input")))))

(test character-selector-before-message-input-streaming
  "Test that character selector appears before message input in streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" t)))
    ;; Find character selector
    (let ((character-selector (search "name=\"character\"" result)))
      (is (not (null character-selector)) "Character selector should exist")
      ;; Find message input
      (let ((message-input (search "name=\"message\"" result)))
        (is (not (null message-input)) "Message input should exist")
        ;; Verify character selector comes before message input
        (is (and character-selector message-input (< character-selector message-input))
            "Character selector should appear before message input")))))

(test character-selector-before-message-input-non-streaming
  "Test that character selector appears before message input in non-streaming version"
  (let ((result (kabotan::generate-character-chat-form "en" nil "dracula" nil)))
    ;; Find character selector
    (let ((character-selector (search "name=\"character\"" result)))
      (is (not (null character-selector)) "Character selector should exist")
      ;; Find message input
      (let ((message-input (search "name=\"message\"" result)))
        (is (not (null message-input)) "Message input should exist")
        ;; Verify character selector comes before message input
        (is (and character-selector message-input (< character-selector message-input))
            "Character selector should appear before message input")))))
