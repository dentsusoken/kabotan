(defsystem "kabotan"
  :description "Kabotan"
  :author "ytsuyoshi <yamashita.tsuyoshi@dentsusoken.com>"
  :license "MIT"
  :serial t
  :depends-on (:ningle
               :clack
               :lack
               :lack-middleware-mount
               :lack-middleware-static
               :lack-middleware-session
               :jonathan
               :cl-ppcre
               :dexador
               :cl-json
               :flexi-streams)
  :components
  ((:module "src"
    :components
            ;; Package definition must be loaded first to define namespace and exports
            ((:file "package")
             
             ;; ========================================
             ;; Utilities Layer
             ;; ========================================
             ;; Core utilities with no internal dependencies
             (:file "utils/validation")        ; Input validation functions
             (:file "utils/error-handling")    ; Error handling and logging
             (:file "utils/response-formatting") ; Response formatting utilities
             (:file "utils/logging")           ; LLM request/response logging
             
             ;; Streaming modules (protocol → handlers)
             ;; Load order: protocol layer before handler layer
             (:file "utils/sse-protocol")      ; SSE protocol utilities (parsing, formatting)
             (:file "utils/streaming-handlers") ; HTMX streaming handlers (depends on sse-protocol)
             (:file "utils/streaming-error-handler") ; Streaming error handling
             (:file "utils/handler-utils")     ; Common handler utilities
             
             ;; HTML template modules (common → forms → features/chat)
             ;; Load order: base components before dependent components
             (:file "utils/html-common")       ; HTML escaping and basic DaisyUI components
             (:file "utils/html-forms")        ; Form components (depends on html-common)
             (:file "utils/html-features")     ; Feature forms (depends on html-common, html-forms)
             (:file "utils/html-chat")         ; Chat components (depends on html-common, html-forms)
             
             ;; ========================================
             ;; Services Layer
             ;; ========================================
             (:file "services/session-manager") ; Server-side session management
             
             ;; LLM service modules (client → retry, client → streaming)
             ;; Load order: core client before retry/streaming wrappers
             (:file "services/llm-client")     ; Core LLM API client and configuration
             (:file "services/llm-retry")      ; Retry logic wrappers (depends on llm-client)
             (:file "services/llm-streaming")  ; Streaming API functions (depends on llm-client)
             
             (:file "services/language-handler") ; Multi-language support
             (:file "services/prompt-builder") ; LLM prompt construction
             
             ;; ========================================
             ;; API Layer
             ;; ========================================
             ;; Feature handlers (return HTML fragments for HTMX)
             (:file "api/handlers/feature-content-handler")    ; Feature form generation
             (:file "api/handlers/monster-diagnostic-handler") ; Monster personality diagnosis
             (:file "api/handlers/story-generator-handler")    ; Halloween story generation
             (:file "api/handlers/character-chat-handler")     ; Character chat with history
             (:file "api/handlers/trivia-bot-handler")         ; Halloween trivia conversations
             (:file "api/handlers/spell-generator-handler")    ; Daily spell generation
             
             ;; API route setup
             (:file "api/halloween-api")       ; API route configuration
             
             ;; ========================================
             ;; Main Application
             ;; ========================================
             (:file "main")                    ; Entry point and server setup
             ))))
