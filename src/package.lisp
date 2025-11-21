(defpackage :kabotan
  (:use
   :cl
   :clack
   :ningle
   :lack
   :lack.middleware.mount
   :lack.middleware.static
   :lack.component)
  (:import-from :flexi-streams
                :make-flexi-stream
                :octets-to-string)
  (:export
   ;; Main entry point
   :main
   
   ;; Error handling utilities
   :handle-api-error
   :format-error-response
   :log-error
   :*suppress-error-logs*
   
   ;; Logging utilities
   :log-llm-request
   :log-llm-response
   :log-llm-error
   :log-llm-retry
   
   ;; Response formatting utilities
   :format-html-response
   :format-monster-diagnostic-response
   :format-story-response
   :format-spell-generator-response
   :format-character-chat-response-with-history
   :format-trivia-response-with-history
   :escape-html
   
   ;; Session management
   :session
   :make-session
   :session-id
   :session-language
   :session-created-at
   :session-last-accessed
   :session-conversations
   :conversation-message
   :make-conversation-message
   :conversation-message-role
   :conversation-message-content
   :conversation-message-timestamp
   :generate-session-id
   :create-session
   :get-session
   :get-or-create-session
   :extract-session-id-from-cookie
   :create-session-cookie
   :get-session-language
   :set-session-language
   :get-conversation-history
   :add-to-conversation-history
   :clear-conversation-history
   :format-conversation-for-llm
   :cleanup-old-sessions
   :maybe-cleanup-sessions
   :get-session-count
   :get-session-info
   :initialize-session-manager
   
   ;; LLM service
   :call-llm
   :call-llm-with-retry
   :call-llm-with-messages
   :call-llm-with-messages-retry
   :validate-message-role
   :messages-to-json-array
   
   ;; Streaming LLM service
   :call-llm-streaming
   :call-llm-with-messages-streaming
   
   ;; Streaming utilities
   :parse-sse-chunk
   :format-sse-data
   :format-sse-error
   :create-sse-response
   :create-streaming-handler
   :handle-streaming-error
   
   ;; Streaming error handler utilities
   :create-streaming-callback-with-tracking
   :handle-streaming-result
   :wrap-streaming-handler
   
   ;; Handler utilities
   :extract-and-validate-language
   :validate-required-params
   :build-validation-error-response
   :build-service-error-response
   :wrap-handler
   
   ;; Language handler
   :*ui-texts*
   :get-ui-text
   :detect-browser-language
   
   ;; Prompt builder
   :build-monster-diagnostic-prompt
   :build-story-generator-prompt
   :build-spell-generator-prompt
   :build-character-chat-system-prompt
   :build-trivia-bot-system-prompt
   :build-story-generator-system-prompt
   
   ;; Validation utilities
   :validate-language
   :validate-non-empty-string
   :validate-story-style
   :sanitize-input
   
   ;; HTML component generators
   :escape-html-char
   :escape-html-attribute
   :sanitize-html-id
   :generate-card
   :generate-alert
   :generate-loading-indicator
   :generate-container
   :generate-error-display
   :generate-result-container
   :generate-form-input
   :generate-form-textarea
   :generate-form-select
   :generate-form-radio-group
   :generate-button
   :generate-form-wrapper
   
   ;; API setup
   :setup-halloween-api
   :get-param
   
   ;; API handlers
   :handle-monster-diagnostic-request
   :handle-monster-diagnostic-streaming
   :handle-story-generator-request
   :handle-story-generator-streaming
   :handle-character-chat-request
   :handle-character-chat-streaming
   :handle-trivia-bot-request
   :handle-trivia-bot-streaming
   :handle-spell-generator-request
   :handle-spell-generator-streaming))
