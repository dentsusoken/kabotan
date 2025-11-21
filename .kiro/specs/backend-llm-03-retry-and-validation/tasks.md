# Implementation Plan

- [x] 1. Create logging utility module ✓ COMPLETED
  - Create `src/utils/logging.lisp` with structured logging functions
  - Implement `log-llm-request`, `log-llm-response`, `log-llm-error`, `log-llm-retry` functions
  - Add timestamp formatting using ISO 8601 format
  - Export logging functions in `src/package.lisp`
  - Already included in `kabotan.asd` system definition
  - All tests passing (167/167)
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 1.5_

- [x] 2. Integrate logging into LLM service ✓ COMPLETED
  - Update `call-openai-api` in `src/services/llm-service.lisp` to log request details before API call
  - Add response logging after successful API calls with timing information
  - Add error logging in error handlers with full context
  - Update `call-llm-with-retry` to log retry attempts with backoff duration
  - Update `kabotan.asd` to include logging.lisp in serial load order
  - All tests passing (167/167)
  - Structured logging now active for all LLM operations
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 3. Add frontend loading indicators
  - Update all feature forms in `public/index.html` to include loading spinner elements
  - Add `hx-indicator` attributes to specify loading elements for each form
  - Ensure loading spinners use `htmx-indicator` class for automatic show/hide
  - Verify CSS rules for `.htmx-request .htmx-indicator` are present
  - Add button disable styling during requests
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

- [x] 3.5. Implement basic character chat and trivia bot API endpoints
  - Create `/api/character-chat` endpoint in `src/api/halloween-api.lisp`
  - Create `/api/trivia-bot` endpoint in `src/api/halloween-api.lisp`
  - Implement basic prompt generation for character chat (without conversation history)
  - Implement basic prompt generation for trivia bot (without conversation history)
  - Use existing `call-llm` function for API calls
  - Return formatted HTML responses for HTMX integration
  - Verify loading indicators work correctly with new endpoints
  - _Requirements: 2.1, 2.2, 2.3, 3.1, 4.1_

- [x] 4. Implement message-based LLM API ✓ COMPLETED
- [x] 4.1 Create message formatting functions ✓ COMPLETED
  - Add `call-llm-with-messages` function in `src/services/llm-service.lisp`
  - Implement message array to JSON conversion
  - Add system prompt prepending logic
  - Validate message roles (system, user, assistant)
  - All tests passing (175/175)
  - Functions exported in `src/package.lisp`
  - _Requirements: 3.1, 3.2, 4.1, 4.2, 4.3_

- [x] 4.2 Update API call implementation ✓ COMPLETED
  - Modify HTTP request body construction to support messages array
  - Add message-based retry wrapper function
  - Export new functions in `src/package.lisp`
  - All tests passing (175/175)
  - `call-llm-with-messages-retry` function implemented with exponential backoff
  - _Requirements: 3.1, 3.2, 5.1, 5.3_

- [x] 5. Add system prompt support to prompt builders ✓ COMPLETED
  - Create system prompt functions in `src/services/prompt-builder.lisp` for each feature
  - Implement `build-character-chat-system-prompt` with character-specific instructions
  - Implement `build-trivia-bot-system-prompt` with educational focus
  - Implement `build-story-generator-system-prompt` with creative writing instructions
  - Export system prompt functions in `src/package.lisp`
  - All tests passing (175/175)
  - System prompt functions implemented for character chat, trivia bot, and story generator
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5_

- [x] 6. Implement frontend conversation history management
- [x] 6.1 Add JavaScript history management
  - Add conversation history object in `public/index.html` JavaScript section
  - Implement `addToHistory`, `clearHistory`, `getHistory` functions
  - Add sessionStorage persistence for history
  - Implement history size limit (10 messages)
  - _Requirements: 3.3, 3.4, 3.5, 3.6_

- [x] 6.2 Update character chat UI for history
  - Modify character chat form to send message history as JSON
  - Update chat message display to show full conversation
  - Add "Clear Chat" button functionality to reset history
  - Update character chat to append new messages to history
  - _Requirements: 3.3, 3.4, 3.5, 3.6_

- [x] 6.3 Update trivia bot UI for history
  - Modify trivia bot form to send message history as JSON
  - Update trivia message display to show full conversation
  - Add "Clear Conversation" button functionality to reset history
  - Update trivia bot to append new messages to history
  - _Requirements: 3.3, 3.4, 3.5, 3.6_

- [x] 7. Update API endpoints to support message history ✓ COMPLETED
- [x] 7.1 Update character chat endpoint ✓ COMPLETED
  - Modify `/api/character-chat` in `src/api/halloween-api.lisp` to accept messages array
  - Parse JSON message history from request
  - Call `call-llm-with-messages` with history and system prompt
  - Update response formatting to work with conversation context
  - All tests passing (175/175)
  - Backward compatibility maintained with legacy single-message API
  - _Requirements: 3.1, 3.2, 4.1, 4.5, 5.4, 5.5_

- [x] 7.2 Update trivia bot endpoint ✓ COMPLETED
  - Modify `/api/trivia-bot` in `src/api/halloween-api.lisp` to accept messages array
  - Parse JSON message history from request
  - Call `call-llm-with-messages` with history and system prompt
  - Update response formatting to work with conversation context
  - All tests passing (175/175)
  - Backward compatibility maintained with legacy single-message API
  - _Requirements: 3.1, 3.2, 4.1, 4.5, 5.4, 5.5_

- [x] 8. Write unit tests for logging ✓ COMPLETED
  - Create test suite in `tests/logging-tests.lisp` for logging functions
  - Test log format output and structure
  - Test timestamp formatting
  - Test severity levels
  - Update `kabotan-test.asd` to include logging tests
  - All tests passing (227/227)
  - Comprehensive test coverage for all logging functions
  - _Requirements: 1.5_

- [x] 9. Write unit tests for message API
  - Create test suite in `tests/message-api-tests.lisp` for message-based functions
  - Test message array to JSON conversion
  - Test system prompt prepending
  - Test role validation
  - Update `kabotan-test.asd` to include message API tests
  - _Requirements: 3.1, 3.2, 4.1, 4.2, 5.1_

- [x] 10. Create E2E tests for conversation features
  - Create `e2e-tests/conversation-history.spec.js` for multi-turn conversation testing
  - Test character chat with multiple messages
  - Test trivia bot with multiple questions
  - Verify loading indicators appear and disappear correctly
  - Test clear history functionality
  - Verify system prompts affect character behavior
  - _Requirements: 2.1, 2.2, 2.3, 3.3, 3.4, 3.5, 3.6, 4.5_
