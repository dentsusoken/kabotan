# Implementation Plan

## Important Note
**During this refactoring period, E2E tests are NOT required for task completion.** The existing E2E tests are based on the old client-side architecture and will fail until the full HTMX refactoring is complete. E2E tests will be updated and validated in task 22 after all features have been migrated to the new architecture.

- [x] 1. Set up HTML template generation infrastructure
  - Create `src/utils/html-templates.lisp` with base template functions
  - Implement HTML escaping and sanitization utilities
  - Create DaisyUI component wrapper functions (card, alert, form elements)
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2. Implement session management system ✅
  - Create `src/services/session-manager.lisp` with session data structures
  - Implement session ID generation and cookie handling
  - Add session storage and retrieval functions
  - Implement language preference storage in sessions
  - Add conversation history storage functions
  - Implement session cleanup mechanism
  - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5, 6.2, 6.3_
  - **Status**: Completed with full test coverage (541/541 tests passing)

- [x] 3. Create feature content endpoint infrastructure
  - Add route definitions for `GET /api/features/*` endpoints
  - Implement base handler for feature content requests
  - Add session ID extraction from cookies
  - Implement language detection from session or Accept-Language header
  - _Requirements: 8.2, 8.3, 8.4_

- [x] 4. Implement spell-generator HTML generation (simplest feature first) ✅
  - Create `generate-spell-generator-form` function in html-templates.lisp
  - Generate form with HTMX attributes (hx-post, hx-target, hx-swap)
  - Include localized labels and placeholders
  - Add loading indicator markup
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 4.2, 4.4_
  - **Status**: Completed with full test coverage (597/597 tests passing)

- [x] 5. Convert spell-generator handler to return HTML
  - Modify `handle-spell-generator-request` to return HTML instead of JSON
  - Update response formatting to generate HTML fragments
  - Include proper content-type headers
  - Add error response HTML generation
  - _Requirements: 1.1, 1.5, 7.1, 7.4_

- [x] 6. Update index.html for HTMX-driven architecture ✅
  - Remove all JavaScript module imports except language-manager.js
  - Add HTMX SSE extension script tag
  - Add HTMX response-targets extension script tag
  - Add hx-ext="sse,response-targets" to body element
  - Update feature tabs with hx-get attributes
  - Remove feature-content div innerHTML, make it empty container
  - Add global error display container
  - _Requirements: 2.1, 4.1, 4.3, 8.1, 8.4, 8.5_
  - **Status**: Completed - All HTMX attributes configured, obsolete JS modules removed

- [x] 7. Simplify language-manager.js
  - Remove all HTML generation functions
  - Keep only localStorage language preference management
  - Add function to update hx-vals with current language
  - Add initial feature load on page load
  - Remove updateUIText and related translation functions
  - _Requirements: 3.1, 3.2, 3.5, 6.1_

- [x] 8. Test spell-generator end-to-end ✅
  - Verify feature loads via HTMX GET request
  - Test form submission returns HTML result
  - Verify language switching reloads content
  - Check error handling displays HTML errors
  - _Requirements: 10.1, 10.4_
  - **Status**: Completed - spell-generator E2E test passing, language switching working, HTML responses validated

- [x] 9. Implement monster-diagnostic HTML generation
  - Create `generate-monster-diagnostic-form` function
  - Generate form with all input fields (favorite_food, sleep_schedule, hobby, fear)
  - Add HTMX attributes for form submission
  - Include localized field labels
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 10. Convert monster-diagnostic handler to return HTML ✅
  - Modify `handle-monster-diagnostic-request` to return HTML
  - Update `format-monster-diagnostic-response` to generate complete HTML fragment
  - Add error response HTML generation
  - _Requirements: 1.1, 1.5, 7.1, 7.4_
  - **Status**: Completed - Handler returns HTML responses, error handling generates HTML alerts

- [x] 11. Implement story-generator HTML generation
  - Create `generate-story-generator-form` function
  - Generate form with name, theme, and style radio buttons
  - Add HTMX attributes for form submission
  - Include localized labels and options
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 12. Convert story-generator handler to return HTML
  - Modify `handle-story-generator-request` to return HTML
  - Update `format-story-response` to generate complete HTML fragment
  - Add error response HTML generation
  - _Requirements: 1.1, 1.5, 7.1, 7.4_

- [x] 13. Implement character-chat HTML generation with history ✅
  - Create `generate-character-chat-form` function
  - Generate chat interface with message history display
  - Add character selection radio buttons
  - Generate message input form with HTMX attributes
  - Include conversation history from session
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 5.2, 5.4_
  - **Status**: Completed with full test coverage (745/745 tests passing)

- [x] 14. Convert character-chat handler to return HTML with history management ✅
  - Modify `handle-character-chat-request` to return HTML
  - Update handler to store messages in session history
  - Retrieve conversation history from session
  - Generate HTML with complete conversation thread
  - Update `format-character-chat-response` to include history
  - Add new message form at bottom of response
  - _Requirements: 1.1, 1.5, 5.1, 5.2, 5.3, 5.4_
  - **Status**: Completed - Handler returns HTML with both user and assistant messages, session history management implemented (746/746 tests passing)

- [x] 15. Implement trivia-bot HTML generation with history
  - Create `generate-trivia-bot-form` function
  - Generate chat interface similar to character-chat
  - Add question input form with HTMX attributes
  - Include conversation history from session
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 5.2, 5.4_

- [x] 16. Convert trivia-bot handler to return HTML with history management ✅
  - Modify `handle-trivia-bot-request` to return HTML
  - Update handler to store Q&A in session history
  - Retrieve conversation history from session
  - Generate HTML with complete Q&A thread
  - Update `format-trivia-response` to include history
  - Add new question form at bottom of response
  - _Requirements: 1.1, 1.5, 5.1, 5.2, 5.3, 5.4_
  - **Status**: Completed - Handler returns HTML with both user and assistant messages, session history management implemented (790/790 tests passing)

- [x] 17. Implement streaming support with HTMX SSE extension
  - Update streaming forms to include sse-connect and sse-swap attributes
  - Modify streaming handlers to send HTML fragments as SSE events
  - Send SSE events with event name "message" for content chunks
  - Send completion event with event name "done"
  - Send error events with event name "error" containing HTML error messages
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

- [x] 18. Convert character-chat streaming to use HTMX SSE with session history ✅
  - Update `generate-character-chat-form` to include streaming option
  - Add sse-connect attribute pointing to streaming endpoint
  - Add sse-swap="message" to content container
  - Modify `handle-character-chat-streaming` to send HTML fragments
  - Integrate session-based conversation history management
  - Send user message HTML as first SSE event
  - Stream assistant response with chat bubble wrapper
  - Accumulate response and save to session history after completion
  - Escape HTML in each chunk before sending
  - Send completion event when done
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 5.1, 5.2, 5.3, 5.4, 10.2_
  - **Status**: Completed - Streaming handler sends HTML fragments as SSE events with proper event names (message, done, error), integrates session history management, sends both user and assistant messages, accumulates and stores responses (803/803 tests passing)

- [x] 19. Convert other streaming features to use HTMX SSE
  - Update monster-diagnostic streaming form and handler
  - Update story-generator streaming form and handler
  - Update trivia-bot streaming form and handler
  - Update spell-generator streaming form and handler
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 10.2_

- [x] 20. Implement language switching endpoint ✅
  - Create `POST /api/set-language` endpoint
  - Update session language preference
  - Return updated feature content in new language
  - Add language parameter to all feature requests
  - _Requirements: 6.1, 6.2, 6.3, 6.4, 6.5_
  - **Status**: Completed - Language switching endpoint implemented in feature-content-handler.lisp, integrated with language-manager.js, session-based language preference management working, all basic language switching E2E tests passing (6/8 tests, 2 failures are due to old architecture and will be fixed in task 22)

- [x] 21. Implement error handling with response-targets
  - Add hx-target-5* attributes to forms for server errors
  - Add hx-target-4* attributes for client errors
  - Generate appropriate error HTML fragments
  - Include retry buttons in error responses
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [x] 22. Update E2E tests for HTML responses
  - Update feature loading tests to expect HTML forms
  - Update form submission tests to expect HTML results
  - Update streaming tests to work with HTMX SSE extension
  - Update language switching tests
  - Update error handling tests
  - _Requirements: 10.1, 10.2, 10.3, 10.4, 10.5_

- [x] 23. Remove obsolete JavaScript files
  - Delete `public/js/feature-content-templates.js`
  - Delete `public/js/feature-ui-handlers.js`
  - Delete `public/js/feature-streaming-handlers.js`
  - Delete `public/js/streaming-manager.js`
  - Delete `public/js/chat-manager.js`
  - Delete `public/js/feature-manager.js`
  - Delete `public/js/chat-streaming.js`
  - Delete `public/js/chat-history.js`
  - Delete `public/js/error-handler.js`
  - Delete `public/js/app-init.js`
  - _Requirements: 3.1, 3.2, 3.3, 3.4_

- [x] 24. Update documentation
  - Update README.md with new architecture description
  - Document HTMX attribute usage patterns
  - Document session management approach
  - Update API documentation to reflect HTML responses
  - Add examples of HTMX SSE extension usage
  - _Requirements: 1.1, 2.1, 5.1, 6.1_

- [ ] 25. Performance optimization
  - Implement HTML template caching
  - Add session cleanup scheduler
  - Optimize HTML generation functions
  - Add performance logging
  - _Requirements: 1.1, 5.1_

- [ ] 26. Security hardening
  - Add CSRF token generation and validation
  - Implement Content Security Policy headers
  - Add rate limiting for API endpoints
  - Audit HTML escaping in all templates
  - _Requirements: 1.1, 7.1_
