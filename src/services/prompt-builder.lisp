(in-package :kabotan)

;;; Prompt Builder
;;;
;;; This module constructs prompts for each Halloween feature mode.
;;; Each prompt builder includes:
;;; - System instructions for the LLM
;;; - Halloween theme context
;;; - Output format requirements
;;; - Language-specific instructions

(defun build-monster-diagnostic-prompt (inputs language)
  "Build a prompt for monster personality diagnosis based on user inputs."
  (let ((favorite-food (getf inputs :favorite-food))
        (sleep-schedule (getf inputs :sleep-schedule))
        (hobby (getf inputs :hobby))
        (fear (getf inputs :fear))
        (lang-instruction (if (string= language "ja")
                              "日本語で回答してください。必ず指定されたフォーマットに従ってください。"
                              "Please respond in English. Follow the specified format exactly.")))
    (format nil "You are an expert Halloween monster personality analyst. Analyze the user's personality traits and determine which Halloween monster they most closely resemble. User's Profile: Favorite food is ~A, Sleep schedule is ~A, Hobby is ~A, Greatest fear is ~A. Provide your analysis in this format: Monster Type: [monster name], Analysis: [3-4 sentences], Fun Fact: [interesting fact]. ~A" 
            favorite-food sleep-schedule hobby fear lang-instruction)))

(defun build-story-generator-prompt (name style elements language)
  "Build a prompt for Halloween story generation with specified style."
  (let ((style-instruction
          (cond
            ((string= style "gothic")
             "Write in a dark, atmospheric Gothic horror style with rich descriptions.")
            ((string= style "parody")
             "Write in a humorous, lighthearted parody style with comedic timing.")
            ((string= style "classic")
             "Write in a traditional classic ghost story style with suspense.")
            (t "Write in a classic Halloween story style.")))
        (lang-instruction (if (string= language "ja")
                              "日本語で物語を書いてください。"
                              "Please write the story in English.")))
    (format nil "You are a skilled Halloween story writer. Create a story with protagonist ~A, theme ~A, style ~A. ~A Write 4-6 paragraphs. ~A" 
            name elements style style-instruction lang-instruction)))

(defun build-character-chat-system-prompt (character language)
  "Build a system prompt for character-based chat."
  (let ((character-persona
          (cond
            ((string= character "dracula")
             "You are Count Dracula, the legendary vampire lord. Speak with old-world elegance and formal vocabulary.")
            ((string= character "witch")
             "You are a wise and mysterious witch. Speak in a cryptic, knowing manner with mystical references.")
            ((string= character "jack")
             "You are Jack-o-Lantern, the cheerful spirit of Halloween. Be enthusiastic and playful.")
            (t "You are a friendly Halloween character.")))
        (lang-instruction (if (string= language "ja")
                              "日本語でキャラクターとして返答してください。"
                              "Please respond as the character in English.")))
    (format nil "~A Keep responses 2-4 sentences. ~A" character-persona lang-instruction)))

(defun build-trivia-bot-system-prompt (language)
  "Build a system prompt for trivia bot."
  (let ((lang-instruction (if (string= language "ja")
                              "日本語で返答してください。"
                              "Please respond in English.")))
    (format nil "You are an enthusiastic Halloween historian and trivia expert. Share fascinating Halloween facts. Keep responses 3-5 sentences. ~A" lang-instruction)))

(defun build-story-generator-system-prompt (style language)
  "Build a system prompt for story generation."
  (let ((style-instruction
          (cond
            ((string= style "gothic")
             "Write in a dark, atmospheric Gothic horror style.")
            ((string= style "parody")
             "Write in a humorous, lighthearted parody style.")
            ((string= style "classic")
             "Write in a traditional classic ghost story style.")
            (t "Write in a classic Halloween story style.")))
        (lang-instruction (if (string= language "ja")
                              "日本語で物語を書いてください。"
                              "Please write the story in English.")))
    (format nil "You are a skilled Halloween story writer. ~A Write 4-6 paragraphs. ~A" style-instruction lang-instruction)))

(defun build-spell-generator-prompt (language)
  "Build a prompt for generating a magical spell or phrase."
  (let ((lang-instruction (if (string= language "ja")
                              "日本語で呪文を作成してください。"
                              "Please create the spell in English.")))
    (format nil "You are an ancient spell weaver. Create a unique magical incantation. Make it mystical and memorable. Provide in this format: Spell: [mystical phrase], Meaning: [2-3 sentences explaining the spell's purpose]. ~A" lang-instruction)))
