# Design Document

## Overview

This design addresses the reorganization of E2E tests to create a clear separation between UI tests (using mocks) and integration tests (using real LLM API). The current test suite has integration tests that use mocks, defeating their purpose. We will reorganize the test directory structure, remove obsolete test infrastructure, and ensure integration tests properly verify end-to-end functionality with real API calls.

## Architecture

### Current Structure
```
e2e-tests/
├── character-chat.spec.js (mixed: UI + [INTEGRATION])
├── monster-diagnostic.spec.js (mixed: UI + [INTEGRATION])
├── story-generator.spec.js (mixed: UI + [INTEGRATION])
├── spell-generator.spec.js (mixed: UI + [INTEGRATION])
├── trivia-bot.spec.js (mixed: UI + [INTEGRATION])
├── streaming-character-chat.spec.js ([INTEGRATION] but uses mocks!)
├── error-handling.spec.js (mixed: UI + [INTEGRATION])
├── language-switching.spec.js (mixed: UI + [INTEGRATION])
├── test-mock-verification.spec.js ([INTEGRATION])
├── test-backend-llm-mock.spec.js (empty file)
└── helpers/
    └── mock-llm-api.js
```

### Proposed Structure
```
e2e-tests/
├── ui/                          # UI tests with mocks (fast)
│   ├── character-chat.spec.js
│   ├── monster-diagnostic.spec.js
│   ├── story-generator.spec.js
│   ├── spell-generator.spec.js
│   ├── trivia-bot.spec.js
│   ├── error-handling.spec.js
│   └── language-switching.spec.js
├── integration/                 # Integration tests without mocks (slow)
│   ├── character-chat-integration.spec.js
│   ├── monster-diagnostic-integration.spec.js
│   ├── story-generator-integration.spec.js
│   ├── spell-generator-integration.spec.js
│   ├── trivia-bot-integration.spec.js
│   ├── streaming-integration.spec.js
│   └── multilingual-integration.spec.js
└── helpers/
    └── mock-llm-api.js
```

## Components and Interfaces

### Test Categories

#### UI Tests (`e2e-tests/ui/`)
- **Purpose**: Verify UI behavior and interactions without requiring real LLM API
- **Characteristics**:
  - Use mocks for all LLM API calls
  - Fast execution (< 60 seconds total)
  - Test UI elements, form validation, navigation
  - Test error handling with mocked errors
  - Test language switching UI behavior
- **Mock Usage**: Required for all tests

#### Integration Tests (`e2e-tests/integration/`)
- **Purpose**: Verify end-to-end functionality with real LLM API
- **Characteristics**:
  - No mocks - use real OpenAI-compatible endpoint
  - Slow execution (up to 180 seconds per test)
  - Test actual LLM responses
  - Test streaming functionality with real SSE
  - Test multilingual support with real translations
- **Mock Usage**: Prohibited

### Test Files

#### UI Test Files
Each UI test file focuses on interface behavior:
- Form element visibility and validation
- Button interactions
- Tab switching
- Error message display
- Loading indicators
- HTML structure verification

#### Integration Test Files

**character-chat-integration.spec.js**
- Test chat with Dracula character (English)
- Test chat with Witch character (Japanese)
- Verify streaming content delivery
- Verify conversation history persistence

**monster-diagnostic-integration.spec.js**
- Test monster diagnostic with English input
- Test monster diagnostic with Japanese input
- Verify streaming response
- Verify result format and content length

**story-generator-integration.spec.js**
- Test story generation with different styles (gothic, parody, classic)
- Test English and Japanese story generation
- Verify streaming content delivery
- Verify story length and format

**spell-generator-integration.spec.js**
- Test spell generation on load
- Test spell regeneration
- Verify streaming content delivery
- Test English and Japanese spell generation

**trivia-bot-integration.spec.js**
- Test trivia question answering
- Test follow-up questions with context
- Verify streaming responses
- Test English and Japanese trivia

**streaming-integration.spec.js**
- Test SSE connection establishment
- Verify incremental content delivery
- Test streaming completion detection
- Verify final content integrity
- Test streaming across all features

**multilingual-integration.spec.js**
- Test language parameter transmission
- Verify English responses
- Verify Japanese responses
- Test language switching during session

## Data Models

### Test Configuration

```javascript
// playwright.config.js updates
module.exports = defineConfig({
  testDir: './e2e-tests',
  projects: [
    {
      name: 'ui-tests',
      testDir: './e2e-tests/ui',
      timeout: 30000, // 30 seconds per test
      use: {
        baseURL: 'http://localhost:5000',
      },
    },
    {
      name: 'integration-tests',
      testDir: './e2e-tests/integration',
      timeout: 180000, // 180 seconds per test
      use: {
        baseURL: 'http://localhost:5000',
      },
    },
  ],
  // ... rest of config
});
```

### Makefile Targets

```makefile
# UI tests - fast, with mocks
test-e2e-ui:
	npx playwright test --project=ui-tests --reporter=list --quiet

# Integration tests - slow, real LLM API
test-e2e-integration:
	npx playwright test --project=integration-tests --reporter=list --quiet

# All E2E tests
test-e2e-all:
	npx playwright test --reporter=list --quiet
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Integration tests never use mocks
*For any* integration test file in `e2e-tests/integration/`, the test SHALL NOT import or use mock-llm-api helper functions
**Validates: Requirements 1.1, 1.3**

### Property 2: UI tests always use mocks
*For any* UI test file in `e2e-tests/ui/` that makes LLM API calls, the test SHALL use mock-llm-api helper functions
**Validates: Requirements 6.3**

### Property 3: Integration tests verify real streaming
*For any* integration test that tests streaming functionality, the test SHALL verify that content is delivered incrementally by observing multiple DOM updates during the response
**Validates: Requirements 5.1, 5.2**

### Property 4: Integration tests cover all core features
*For any* core feature (monster-diagnostic, story-generator, character-chat, spell-generator, trivia-bot), there SHALL exist at least one integration test that verifies the feature with real LLM API
**Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5**

### Property 5: Integration tests verify multilingual support
*For any* integration test suite, there SHALL exist tests that verify both English and Japanese language support with real LLM responses
**Validates: Requirements 4.1, 4.2, 4.4**

### Property 6: Test directory structure is separated
*For any* test file, it SHALL be located in either `e2e-tests/ui/` or `e2e-tests/integration/` directory, not in the root `e2e-tests/` directory
**Validates: Requirements 6.3, 6.5**

### Property 7: Obsolete test infrastructure is removed
*For any* test file or make target, if it is empty or non-functional, it SHALL NOT exist in the repository
**Validates: Requirements 2.1, 2.2, 2.3, 2.4**

## Error Handling

### Integration Test Failures
- **LLM API unavailable**: Test should fail with clear error message indicating API connection issue
- **Timeout**: Test should fail after 180 seconds with timeout error
- **Invalid response**: Test should fail if LLM returns malformed or empty response
- **Streaming interruption**: Test should fail if SSE connection drops before completion

### UI Test Failures
- **Mock not configured**: Test should fail with clear error if mock is not properly set up
- **UI element not found**: Test should fail with selector error
- **Validation not working**: Test should fail if HTML5 validation is not enforced

## Testing Strategy

### Unit Testing
Not applicable - this is a test reorganization project, not application code.

### Property-Based Testing
Not applicable - E2E tests are example-based by nature.

### Integration Testing
The integration tests themselves are the deliverable. They will be verified by:
1. Running integration tests with real LLM API and confirming they pass
2. Verifying no mock imports exist in integration test files
3. Confirming streaming behavior is properly tested
4. Validating multilingual support is tested

### Manual Testing
1. Run `make test-e2e-ui` and verify all tests pass quickly (< 60 seconds)
2. Run `make test-e2e-integration` with valid OPENAI_API_KEY and verify tests pass
3. Verify integration tests fail gracefully when API key is invalid
4. Confirm test output clearly indicates which tests are UI vs integration

## Implementation Notes

### File Migration Strategy
1. Create new directory structure (`ui/` and `integration/`)
2. Copy existing test files to `ui/` directory
3. Create new integration test files in `integration/` directory
4. Extract integration test cases from mixed files
5. Remove mock usage from integration tests
6. Update imports and paths
7. Delete obsolete files
8. Update Playwright configuration
9. Update Makefile targets
10. Update documentation

### Mock Removal from Integration Tests
Current files with problematic mock usage:
- `streaming-character-chat.spec.js`: Uses `mockStreamingEndpoint` and `mockNonStreamingEndpoint`
- `spell-generator.spec.js`: Uses mocks in [INTEGRATION] tests
- `story-generator.spec.js`: Uses mocks in [INTEGRATION] tests

These tests need to be rewritten to:
1. Remove all mock imports
2. Remove mock setup in beforeEach
3. Use real API endpoints
4. Adjust timeouts for real LLM response times
5. Add proper error handling for API failures

### Streaming Verification Strategy
Integration tests should verify streaming by:
1. Observing the target element before submission
2. Using MutationObserver or polling to detect content changes
3. Verifying multiple updates occur (not just final state)
4. Confirming final content is complete
5. Measuring approximate time to ensure streaming occurred (not instant)

Example approach:
```javascript
// Track content changes
const contentChanges = [];
const observer = new MutationObserver(() => {
  contentChanges.push(element.textContent.length);
});
observer.observe(element, { childList: true, subtree: true });

// Submit form
await page.click('button[type="submit"]');

// Wait for completion
await page.waitForFunction(() => /* completion condition */);

// Verify streaming occurred
expect(contentChanges.length).toBeGreaterThan(1); // Multiple updates
expect(contentChanges).toEqual(expect.arrayContaining([
  expect.any(Number),
  expect.any(Number),
]));
```

### Configuration Updates

**playwright.config.js**
- Add projects for ui-tests and integration-tests
- Set different timeouts for each project
- Keep single webServer configuration

**Makefile**
- Remove `test-e2e-streaming` target (obsolete)
- Update `test-e2e-ui` to use `--project=ui-tests`
- Update `test-e2e-integration` to use `--project=integration-tests`
- Keep `test-e2e-all` for running both

**package.json**
- Update npm scripts to match new structure
- Add separate scripts for ui and integration tests

### Documentation Updates
- Update `e2e-tests/README.md` with new structure
- Document the difference between UI and integration tests
- Provide examples of when to add tests to each category
- Document how to run each test category
