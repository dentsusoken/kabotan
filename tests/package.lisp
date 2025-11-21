(defpackage :kabotan.tests
  (:use :cl :fiveam)
  (:import-from :kabotan
                :main
                :format-error-response
                :log-error
                :*suppress-error-logs*
                :format-html-response
                :format-spell-generator-response
                :get-ui-text
                :detect-browser-language
                :build-spell-generator-prompt
                :call-llm
                :call-llm-with-retry
                :call-llm-with-messages
                :call-llm-with-messages-retry
                :setup-halloween-api
                :get-param
                :parse-sse-chunk
                :format-sse-data
                :format-sse-error
                :create-sse-response
                :handle-monster-diagnostic-streaming
                :handle-story-generator-streaming
                :handle-character-chat-streaming
                :handle-trivia-bot-streaming
                :handle-spell-generator-streaming)
  (:import-from :cl-ppcre
                :scan)
  (:export
   :run-tests))
