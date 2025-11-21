(in-package :kabotan)

;;; Halloween feature API endpoints

(defun setup-halloween-api (api)
  "Setup all Halloween feature API endpoints"
  
  ;; Feature content endpoints (GET)
  (setf (ningle:route api "/features/monster-diagnostic" :method :GET)
        #'handle-get-monster-diagnostic)
  
  (setf (ningle:route api "/features/story-generator" :method :GET)
        #'handle-get-story-generator)
  
  (setf (ningle:route api "/features/character-chat" :method :GET)
        #'handle-get-character-chat)
  
  (setf (ningle:route api "/features/trivia-bot" :method :GET)
        #'handle-get-trivia-bot)
  
  (setf (ningle:route api "/features/spell-generator" :method :GET)
        #'handle-get-spell-generator)
  
  ;; Language switching endpoint
  (setf (ningle:route api "/set-language" :method :POST)
        #'handle-set-language)
  
  ;; Monster Diagnostic endpoint
  (setf (ningle:route api "/monster-diagnostic" :method :POST)
        #'handle-monster-diagnostic-request)
  
  ;; Monster Diagnostic streaming endpoint
  (setf (ningle:route api "/monster-diagnostic-stream" :method :GET)
        #'handle-monster-diagnostic-streaming)
  
  ;; Story Generator endpoint
  (setf (ningle:route api "/story-generator" :method :POST)
        #'handle-story-generator-request)
  
  ;; Story Generator streaming endpoint
  (setf (ningle:route api "/story-generator-stream" :method :GET)
        #'handle-story-generator-streaming)
  
  ;; Character Chat endpoint
  (setf (ningle:route api "/character-chat" :method :POST)
        #'handle-character-chat-request)
  
  ;; Character Chat streaming endpoint
  (setf (ningle:route api "/character-chat-stream" :method :GET)
        #'handle-character-chat-streaming)
  
  ;; Trivia Bot endpoint
  (setf (ningle:route api "/trivia-bot" :method :POST)
        #'handle-trivia-bot-request)
  
  ;; Trivia Bot streaming endpoint
  (setf (ningle:route api "/trivia-bot-stream" :method :GET)
        #'handle-trivia-bot-streaming)
  
  ;; Spell Generator endpoint
  (setf (ningle:route api "/spell-generator" :method :POST)
        #'handle-spell-generator-request)
  
  ;; Spell Generator streaming endpoint
  (setf (ningle:route api "/spell-generator-stream" :method :GET)
        #'handle-spell-generator-streaming))
