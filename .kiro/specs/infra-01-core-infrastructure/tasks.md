# Implementation Plan

## Core Infrastructure Status

All core infrastructure tasks have been completed. The system now has:

- ✅ Complete project structure with proper ASDF system definition
- ✅ Environment-based configuration for LLM service
- ✅ Full utilities layer (validation, error handling, response formatting, logging, streaming)
- ✅ Complete service layer (LLM service with streaming, language handler, prompt builder)
- ✅ Frontend base structure with TailwindCSS, DaisyUI, and HTMX
- ✅ Language switching functionality with browser detection
- ✅ Streaming support for real-time LLM responses

## Completed Tasks

- [x] 1. Set up project structure and update ASDF system definition
  - Create directory structure for api/, services/, and utils/ under src/
  - Update kabotan.asd to include new source files in correct load order
  - Update package.lisp to export necessary symbols
  - _Requirements: All requirements depend on proper project structure_

- [x] 2. Implement environment-based configuration
  - [x] 2.1 Configure LLM service to read from environment variables
    - Use OPENAI_API_KEY, OPENAI_MODEL, and OPENAI_HOST directly in llm-service
    - Provide sensible defaults for development
    - Support ENABLE_STREAMING, STREAMING_TIMEOUT, and STREAMING_UPDATE_INTERVAL
  - [x] 2.2 Removed setup-openai-api-setting function (no longer needed)
    - LLM service now reads environment variables directly
    - Removed openai package dependency

- [x] 3. Implement utilities layer
  - [x] 3.1 Create validation.lisp with input validation functions
    - Implement validate-language, validate-non-empty-string
    - Implement sanitize-input for security
    - Implement validate-messages-json for chat message validation
    - Implement validate-required-fields for multi-field validation
    - _Requirements: 4.1, 4.3_
  - [x] 3.2 Create error-handling.lisp with error handling utilities
    - Implement handle-api-error, format-error-response, log-error
    - _Requirements: 4.2, 4.4, 4.5_
  - [x] 3.3 Create response-formatting.lisp with HTML response formatters
    - Implement format-html-response
    - Use DaisyUI classes for styling
    - Implement feature-specific formatters (monster, story, chat, trivia, spell)
    - _Requirements: 3.4_
  - [x] 3.4 Create logging.lisp with structured logging utilities
    - Implement log-llm-request, log-llm-response, log-llm-error, log-llm-retry
    - _Requirements: 4.2_
  - [x] 3.5 Create streaming.lisp with SSE utilities
    - Implement parse-sse-chunk, format-sse-data, format-sse-error
    - Implement create-sse-response, create-streaming-handler
    - _Requirements: 1.2, 3.3_
  - [x] 3.6 Create streaming-error-handler.lisp with streaming error handling
    - Implement create-streaming-callback-with-tracking
    - Implement handle-streaming-result, wrap-streaming-handler
    - _Requirements: 4.2, 4.4_
  - [x] 3.7 Create handler-utils.lisp with common handler utilities
    - Implement extract-and-validate-language
    - Implement validate-required-params, build-validation-error-response
    - Implement wrap-handler for consistent error handling
    - _Requirements: 4.1, 4.2_

- [x] 4. Implement service layer
  - [x] 4.1 Implement llm-service.lisp with LLM interaction functions
    - [x] Implement call-llm function with timeout handling using dexador HTTP client
    - [x] Implement call-llm-with-retry with exponential backoff and error handling
    - [x] Implement call-llm-with-messages for conversational interactions
    - [x] Implement call-llm-with-messages-retry for reliable conversational interactions
    - [x] Implement streaming functions: call-llm-streaming, call-llm-with-messages-streaming
    - [x] Support test mocking via dynamic variables
    - _Requirements: 1.2, 1.3, 4.4_
  - [x] 4.2 Implement language-handler.lisp with language management
    - Define *ui-texts* parameter with Japanese and English text for all UI elements
    - Implement get-ui-text function to retrieve translations
    - Implement detect-browser-language function to parse Accept-Language header
    - _Requirements: 2.1, 2.2, 2.5_
  - [x] 4.3 Implement prompt-builder.lisp with base prompt utilities
    - Implement base prompt construction functions
    - Implement Halloween theme context injection
    - Implement language-specific prompt formatting
    - Implement system prompt builders for character chat and trivia bot
    - _Requirements: 1.1, 2.3, 2.4_

- [x] 5. Implement frontend base structure
  - [x] 5.1 Create main HTML structure in public/index.html
    - Replace existing HTML with complete structure including TailwindCSS and DaisyUI via CDN
    - Include HTMX library via CDN
    - Set up dark mode theme with Halloween colors (orange, purple, black)
    - Create language selector toggle (Japanese/English) in header
    - Create feature mode navigation using DaisyUI tabs component
    - Create dynamic content container for feature components
    - Add streaming indicator and response templates
    - Add mobile responsive styles
    - _Requirements: 2.1, 3.1, 3.2_
  - [x] 5.2 Add loading indicators and error displays
    - Implement DaisyUI loading spinner with htmx:indicator class for HTMX requests
    - Style error messages with DaisyUI alert component (alert-error class)
    - Add streaming-specific loading indicators
    - _Requirements: 3.3, 3.4_
  - [x] 5.3 Create custom-styles.css for Halloween theme overrides
    - Define custom CSS color variables for Halloween theme
    - Add any additional styling not covered by DaisyUI
    - Link stylesheet in index.html
    - _Requirements: 3.4_

- [x] 6. Implement language switching functionality
  - [x] 6.1 Wire up language toggle to update UI text
    - Add JavaScript to handle language toggle click events
    - Use vanilla JS to swap text content for all UI elements
    - Store language preference in localStorage
    - Update data-lang attribute on html element
    - _Requirements: 2.1, 2.2_
  - [x] 6.2 Ensure all API calls include language parameter
    - Add hidden input fields to include current language in HTMX requests
    - Update all forms to include language parameter
    - _Requirements: 2.3, 2.4_
  - [x] 6.3 Implement browser language detection on first load
    - Add JavaScript to read navigator.language on page load
    - Set initial language based on detection (ja for Japanese, en otherwise)
    - Apply initial language to UI
    - _Requirements: 2.5_

- [x] 7. Implement JavaScript modules for frontend functionality
  - [x] 7.1 Create language-manager.js
    - Implement initLanguage, updateUIText, getText functions
    - Implement setupLanguageToggle for language switching
    - _Requirements: 2.1, 2.2, 2.5_
  - [x] 7.2 Create streaming-manager.js
    - Implement StreamingManager class for SSE connections
    - Handle streaming start, stop, error, and completion
    - Support browser compatibility detection
    - _Requirements: 1.2, 3.3, 4.4_
  - [x] 7.3 Create feature-manager.js
    - Implement switchFeature for tab navigation
    - Handle feature loading and context clearing
    - _Requirements: 3.1, 3.2, 3.5_
  - [x] 7.4 Create chat-manager.js
    - Implement conversation history management
    - Handle message display and formatting
    - _Requirements: Feature-specific (Character Chat, Trivia Bot)_
  - [x] 7.5 Create error-handler.js
    - Implement global error handling
    - Handle streaming errors and fallbacks
    - _Requirements: 4.2, 4.5_

## Notes

The core infrastructure is complete and fully functional. All requirements from the requirements document have been addressed:

- **Requirement 1**: Halloween-themed responses with LLM integration ✅
- **Requirement 2**: Bilingual support (Japanese/English) with browser detection ✅
- **Requirement 3**: Responsive and intuitive interface with feature navigation ✅
- **Requirement 4**: Reliable processing with validation, error handling, and retry logic ✅

Additional features implemented beyond the original requirements:
- Streaming support for real-time LLM responses
- Comprehensive logging and monitoring
- Mobile-responsive design
- Browser compatibility detection
- Structured error handling with user-friendly messages

The infrastructure is ready to support all Halloween features (Monster Diagnostic, Story Generator, Character Chat, Trivia Bot, Spell Generator).
