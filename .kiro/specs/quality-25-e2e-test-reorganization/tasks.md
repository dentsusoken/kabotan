# Implementation Plan

- [x] 1. Create new test directory structure
  - Create `e2e-tests/ui/` directory for UI tests with mocks
  - Create `e2e-tests/integration/` directory for integration tests without mocks
  - _Requirements: 6.3, 6.5_

- [x] 2. Remove obsolete test infrastructure
  - Delete empty file `e2e-tests/test-backend-llm-mock.spec.js`
  - Remove `test-e2e-streaming` target from Makefile
  - _Requirements: 2.1, 2.2, 2.3, 2.4_

- [x] 3. Create UI test files by extracting UI-focused tests
  - Copy and adapt `character-chat.spec.js` to `e2e-tests/ui/character-chat.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `monster-diagnostic.spec.js` to `e2e-tests/ui/monster-diagnostic.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `story-generator.spec.js` to `e2e-tests/ui/story-generator.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `spell-generator.spec.js` to `e2e-tests/ui/spell-generator.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `trivia-bot.spec.js` to `e2e-tests/ui/trivia-bot.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `error-handling.spec.js` to `e2e-tests/ui/error-handling.spec.js` (remove [INTEGRATION] tests)
  - Copy and adapt `language-switching.spec.js` to `e2e-tests/ui/language-switching.spec.js` (remove [INTEGRATION] tests)
  - _Requirements: 6.3, 6.5_

- [x] 4. Create integration test for character chat
  - Create `e2e-tests/integration/character-chat-integration.spec.js`
  - Remove all mock imports and setup
  - Test chat with Dracula character in English
  - Test chat with Witch character in Japanese
  - Verify streaming content delivery with real LLM
  - Verify conversation history persistence
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 3.3, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4_

- [x] 5. Create integration test for monster diagnostic
  - Create `e2e-tests/integration/monster-diagnostic-integration.spec.js`
  - Remove all mock imports and setup
  - Test monster diagnostic with English input
  - Test monster diagnostic with Japanese input
  - Verify streaming response with real LLM
  - Verify result format and content length
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 3.1, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4_

- [x] 6. Create integration test for story generator
  - Create `e2e-tests/integration/story-generator-integration.spec.js`
  - Remove all mock imports and setup
  - Test story generation with gothic style in English
  - Test story generation with parody style in Japanese
  - Verify streaming content delivery with real LLM
  - Verify story length and format
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 3.2, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4_

- [x] 7. Create integration test for spell generator
  - Create `e2e-tests/integration/spell-generator-integration.spec.js`
  - Remove all mock imports and setup
  - Test spell generation on load in English
  - Test spell regeneration in Japanese
  - Verify streaming content delivery with real LLM
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 3.4, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4_

- [x] 8. Create integration test for trivia bot
  - Create `e2e-tests/integration/trivia-bot-integration.spec.js`
  - Remove all mock imports and setup
  - Test trivia question answering in English
  - Test follow-up questions with context in Japanese
  - Verify streaming responses with real LLM
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 3.5, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4_

- [x] 9. Create comprehensive streaming integration test
  - Create `e2e-tests/integration/streaming-integration.spec.js`
  - Test SSE connection establishment across all features
  - Verify incremental content delivery (multiple DOM updates)
  - Test streaming completion detection
  - Verify final content integrity
  - Use MutationObserver or polling to detect streaming behavior
  - _Requirements: 5.1, 5.2, 5.3, 5.4_

- [x] 10. Create multilingual integration test
  - Create `e2e-tests/integration/multilingual-integration.spec.js`
  - Test language parameter transmission to API
  - Verify English responses from LLM
  - Verify Japanese responses from LLM
  - Test language switching during session
  - _Requirements: 4.1, 4.2, 4.3, 4.4_

- [x] 11. Update Playwright configuration
  - Add `ui-tests` project with testDir `./e2e-tests/ui` and timeout 30000
  - Add `integration-tests` project with testDir `./e2e-tests/integration` and timeout 180000
  - Keep single webServer configuration
  - _Requirements: 6.1, 6.2, 6.4_

- [x] 12. Update Makefile targets
  - Update `test-e2e-ui` to use `--project=ui-tests`
  - Update `test-e2e-integration` to use `--project=integration-tests`
  - Add `test-e2e-all` to run both projects
  - _Requirements: 6.4_

- [x] 13. Update package.json scripts
  - Update npm scripts to match new directory structure
  - Add separate scripts for ui and integration tests
  - _Requirements: 6.4_

- [x] 14. Delete original mixed test files
  - Delete `e2e-tests/character-chat.spec.js`
  - Delete `e2e-tests/monster-diagnostic.spec.js`
  - Delete `e2e-tests/story-generator.spec.js`
  - Delete `e2e-tests/spell-generator.spec.js`
  - Delete `e2e-tests/trivia-bot.spec.js`
  - Delete `e2e-tests/streaming-character-chat.spec.js`
  - Delete `e2e-tests/error-handling.spec.js`
  - Delete `e2e-tests/language-switching.spec.js`
  - Delete `e2e-tests/test-mock-verification.spec.js`
  - _Requirements: 6.3, 6.5_

- [x] 15. Update E2E test documentation
  - Update `e2e-tests/README.md` with new directory structure
  - Document the difference between UI and integration tests
  - Provide examples of when to add tests to each category
  - Document how to run each test category
  - _Requirements: 6.3, 6.4, 6.5_

- [x] 16. Checkpoint - Verify test organization
  - Run `make test-e2e-ui` and verify all UI tests pass quickly (< 60 seconds)
  - Run `make test-e2e-integration` with valid OPENAI_API_KEY and verify integration tests pass
  - Verify no mock imports exist in integration test files
  - Confirm test output clearly indicates which tests are UI vs integration
  - Ensure all tests pass, ask the user if questions arise
