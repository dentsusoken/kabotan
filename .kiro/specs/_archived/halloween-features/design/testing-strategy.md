# Testing Strategy

## Unit Testing (FiveAM)

### Service Layer Tests (`tests/services/`)

Test files:
- `llm-service-test.lisp`: Test LLM service calls with mocked API function
- `prompt-builder-test.lisp`: Test prompt construction for all feature modes
- `language-handler-test.lisp`: Test language detection and UI text retrieval

Test coverage:
- Prompt builders produce correct format for each feature mode
- Language handler returns correct text for all supported keys
- LLM service handles timeouts and retries correctly
- Error conditions are handled appropriately

### Utilities Tests (`tests/utils/`)

Test files:
- `validation-test.lisp`: Test all validation functions
- `response-formatting-test.lisp`: Test HTML response formatting

Test coverage:
- Validation functions correctly accept valid inputs
- Validation functions correctly reject invalid inputs
- Input sanitization removes dangerous content
- Response formatters produce valid HTML

### API Layer Tests (`tests/api/`)

Test files:
- `halloween-api-test.lisp`: Test all API endpoints with mocked services

Test coverage:
- Each endpoint accepts valid requests and returns correct responses
- Each endpoint rejects invalid requests with appropriate errors
- Each endpoint handles service layer errors gracefully
- Response format matches HTMX expectations

## Integration Testing

### LLM Integration Tests (`tests/integration/`)

Test file: `llm-integration-test.lisp`

Test coverage:
- Real calls to LLM service (if available in test environment)
- End-to-end flow from API request to LLM response
- Timeout and retry behavior with real network conditions

## End-to-End Testing (Playwright)

### E2E Test Scenarios (`tests/e2e/`)

Test files:
- `monster-diagnostic.spec.js`: Test monster diagnostic feature flow
- `story-generator.spec.js`: Test story generation feature flow
- `character-chat.spec.js`: Test character chat feature flow
- `trivia-bot.spec.js`: Test trivia bot feature flow
- `spell-generator.spec.js`: Test spell generator feature flow with regeneration
- `language-switching.spec.js`: Test language switching functionality

Test coverage:
- User can navigate to each feature mode
- User can submit inputs and receive responses
- User can switch languages and see updated UI
- User can regenerate spells multiple times in spell generator
- Spell content changes on each regeneration
- Error messages display correctly when LLM service fails
- Loading indicators appear during processing
- HTMX interactions work correctly

## Test Execution Strategy

1. **Development**: Run unit tests frequently during implementation
2. **Pre-commit**: Run all unit tests before committing code
3. **CI Pipeline**: Run unit tests + integration tests on every push
4. **Pre-deployment**: Run full test suite including E2E tests
5. **Test-First Approach**: Write tests before implementing features (per development.md guidelines)

## Mocking Strategy

- Mock LLM API function for unit tests to avoid external dependencies
- Use test fixtures for common LLM responses
- Mock HTTP requests in API layer tests
- Use real LLM service only in integration tests (when available)
