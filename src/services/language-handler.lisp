(in-package :kabotan)

;;; Language Management
;;;
;;; This module provides language handling functionality including:
;;; - UI text translations for Japanese and English
;;; - Translation retrieval by key and language
;;; - Browser language detection from Accept-Language header

(defparameter *ui-texts*
  '((:ja . ((:app-title . "ハロウィン体験")
            (:language-selector . "言語")
            (:feature-monster-diagnostic . "モンスター診断")
            (:feature-story-generator . "ストーリー生成")
            (:feature-character-chat . "キャラクターチャット")
            (:feature-trivia-bot . "トリビアボット")
            (:feature-spell-generator . "呪文生成")
            
            ;; Monster Diagnostic
            (:monster-diagnostic-title . "モンスター診断")
            (:monster-diagnostic-description . "あなたの性格に合ったハロウィンモンスターを診断します")
            (:monster-favorite-food . "好きな食べ物")
            (:monster-sleep-schedule . "睡眠スケジュール")
            (:monster-hobby . "趣味")
            (:monster-fear . "恐れているもの")
            (:monster-submit . "診断する")
            
            ;; Story Generator
            (:story-title . "ハロウィンストーリー生成")
            (:story-description . "あなただけのハロウィンストーリーを作成します")
            (:story-name . "名前")
            (:story-theme . "テーマ")
            (:story-style . "スタイル")
            (:story-style-gothic . "ゴシック")
            (:story-style-parody . "パロディ")
            (:story-style-classic . "クラシック")
            (:story-submit . "生成する")
            
            ;; Character Chat
            (:chat-title . "キャラクターチャット")
            (:chat-description . "ハロウィンキャラクターと会話しましょう")
            (:chat-character . "キャラクター")
            (:chat-character-dracula . "ドラキュラ")
            (:chat-character-witch . "魔女")
            (:chat-character-jack . "ジャック・オー・ランタン")
            (:chat-message . "メッセージ")
            (:chat-submit . "送信")
            
            ;; Trivia Bot
            (:trivia-title . "ハロウィントリビア")
            (:trivia-description . "ハロウィンの豆知識を学びましょう")
            (:trivia-question . "質問")
            (:trivia-submit . "送信")
            
            ;; Spell Generator
            (:spell-title . "呪文生成")
            (:spell-description . "あなたのための魔法の言葉を生成します")
            (:spell-phrase . "呪文")
            (:spell-explanation . "説明")
            (:spell-regenerate . "新しい呪文を生成")
            
            ;; Common
            (:loading . "読み込み中...")
            (:error-occurred . "エラーが発生しました")
            (:validation-error . "入力内容を確認してください")
            (:api-error . "APIエラーが発生しました")
            (:network-error . "ネットワークエラーが発生しました")
            (:retry . "再試行")
            
            ;; Streaming
            (:streaming-generating . "応答を生成中...")
            (:streaming-stop . "停止")
            (:streaming-error . "ストリーミングエラーが発生しました")
            (:streaming-connection-error . "接続エラーが発生しました")
            (:streaming-retry . "再試行")))
    
    (:en . ((:app-title . "Halloween Experience")
            (:language-selector . "Language")
            (:feature-monster-diagnostic . "Monster Diagnostic")
            (:feature-story-generator . "Story Generator")
            (:feature-character-chat . "Character Chat")
            (:feature-trivia-bot . "Trivia Bot")
            (:feature-spell-generator . "Spell Generator")
            
            ;; Monster Diagnostic
            (:monster-diagnostic-title . "Monster Diagnostic")
            (:monster-diagnostic-description . "Discover which Halloween monster matches your personality")
            (:monster-favorite-food . "Favorite Food")
            (:monster-sleep-schedule . "Sleep Schedule")
            (:monster-hobby . "Hobby")
            (:monster-fear . "What You Fear")
            (:monster-submit . "Diagnose")
            
            ;; Story Generator
            (:story-title . "Halloween Story Generator")
            (:story-description . "Create your own Halloween story")
            (:story-name . "Name")
            (:story-theme . "Theme")
            (:story-style . "Style")
            (:story-style-gothic . "Gothic")
            (:story-style-parody . "Parody")
            (:story-style-classic . "Classic")
            (:story-submit . "Generate")
            
            ;; Character Chat
            (:chat-title . "Character Chat")
            (:chat-description . "Chat with Halloween characters")
            (:chat-character . "Character")
            (:chat-character-dracula . "Dracula")
            (:chat-character-witch . "Witch")
            (:chat-character-jack . "Jack-o'-Lantern")
            (:chat-message . "Message")
            (:chat-submit . "Send")
            
            ;; Trivia Bot
            (:trivia-title . "Halloween Trivia")
            (:trivia-description . "Learn interesting facts about Halloween")
            (:trivia-question . "Question")
            (:trivia-submit . "Send")
            
            ;; Spell Generator
            (:spell-title . "Spell Generator")
            (:spell-description . "Generate a magical phrase for you")
            (:spell-phrase . "Spell")
            (:spell-explanation . "Explanation")
            (:spell-regenerate . "Generate New Spell")
            
            ;; Common
            (:loading . "Loading...")
            (:error-occurred . "An error occurred")
            (:validation-error . "Please check your input")
            (:api-error . "API error occurred")
            (:network-error . "Network error occurred")
            (:retry . "Retry")
            
            ;; Streaming
            (:streaming-generating . "Generating response...")
            (:streaming-stop . "Stop")
            (:streaming-error . "Streaming error occurred")
            (:streaming-connection-error . "Connection error occurred")
            (:streaming-retry . "Retry"))))
  "UI text translations for Japanese and English.
   Structure: ((language . ((key . text) ...)) ...)")

(defun get-ui-text (key language)
  "Retrieve UI text for the given key and language.
   
   Parameters:
   - key: Keyword identifying the UI text (e.g., :app-title)
   - language: Language code string (\"ja\" or \"en\")
   
   Returns:
   - String containing the translated text, or nil if not found
   
   Example:
   (get-ui-text :app-title \"ja\") => \"ハロウィン体験\"
   (get-ui-text :app-title \"en\") => \"Halloween Experience\""
  (let* ((lang-keyword (cond
                         ((string= language "ja") :ja)
                         ((string= language "en") :en)
                         (t :en))) ; Default to English
         (lang-texts (cdr (assoc lang-keyword *ui-texts*))))
    (cdr (assoc key lang-texts))))

(defun detect-browser-language (accept-language-header)
  "Parse Accept-Language header and return detected language code.
   
   Parameters:
   - accept-language-header: String containing Accept-Language HTTP header value
     (e.g., \"ja,en-US;q=0.9,en;q=0.8\")
   
   Returns:
   - String \"ja\" if Japanese is detected, \"en\" otherwise
   
   The function looks for language codes in the Accept-Language header and
   returns \"ja\" if any Japanese language code is found (ja, ja-JP, etc.),
   otherwise defaults to \"en\".
   
   Example:
   (detect-browser-language \"ja,en-US;q=0.9\") => \"ja\"
   (detect-browser-language \"en-US,en;q=0.9\") => \"en\"
   (detect-browser-language nil) => \"en\""
  (if (and accept-language-header
           (stringp accept-language-header))
      (let ((languages (cl-ppcre:split "," accept-language-header)))
        ;; Check each language in the list
        (if (some (lambda (lang)
                    ;; Extract language code before semicolon or hyphen
                    (let ((code (string-trim '(#\Space #\Tab)
                                           (first (cl-ppcre:split "[;-]" lang)))))
                      (string-equal code "ja")))
                  languages)
            "ja"
            "en"))
      "en")) ; Default to English if header is nil or invalid
