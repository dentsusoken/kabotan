# Mock Test Verification - Requirements

## Overview
Verify that all E2E mock tests are properly configured and do not make actual LLM API calls.

## Problem Statement
Currently, some E2E tests marked as "mock tests" are still making actual calls to the LLM API server, as evidenced by:
- LLM server logs showing API requests during mock test execution
- 429 rate limit errors appearing during test runs
- Tests taking longer than expected (30-60s instead of 1-2s)

## Goals
1. Ensure all mock tests use mocked endpoints (both streaming and non-streaming)
2. Verify no actual LLM API calls are made during mock test execution
3. Reduce mock test execution time to 3-5 minutes total
4. Maintain clear separation between mock tests and integration tests

## Success Criteria
1. Running `npx playwright test --grep-invert "\[INTEGRATION\]"` produces:
   - No LLM API logs in application server output
   - No 429 rate limit errors
   - Completion time: 3-5 minutes for all ~87 tests
   - 98%+ success rate

2. Each test file properly mocks:
   - Non-streaming endpoints (`/api/FEATURE`)
   - Streaming endpoints (`/api/FEATURE-stream`)

3. Documentation clearly explains:
   - Which tests use mocks vs real API
   - How to verify mock configuration
   - How to add mocks to new tests

## Test File Categories

### Category 1: Feature Tests (5 files)
- `monster-diagnostic.spec.js`
- `story-generator.spec.js`
- `character-chat.spec.js`
- `spell-generator.spec.js`
- `trivia-bot.spec.js`

**Requirements**: Must mock both streaming and non-streaming endpoints

### Category 2: Streaming Tests (2 files)
- `streaming-character-chat.spec.js` (covers all 5 features)
- `streaming-error-handling.spec.js`

**Requirements**: Must mock both streaming and non-streaming endpoints for all features

### Category 3: Cross-Feature Tests (4 files)
- `conversation-history.spec.js`
- `language-switching.spec.js`
- `error-handling.spec.js`
- `browser-compatibility.spec.js`

**Requirements**: Must use `mockAllEndpoints()` or mock specific endpoints used

### Category 4: Integration Tests (1 file)
- `integration.spec.js`

**Requirements**: Should NOT use mocks (uses real LLM API)

## Verification Method
For each test file or group:
1. Clear application logs
2. Run specific test file: `npx playwright test e2e-tests/FILE.spec.js --grep-invert "\[INTEGRATION\]"`
3. Check application logs for LLM API calls
4. If LLM calls found: identify missing mocks and fix
5. Re-run and verify no LLM calls

## Out of Scope
- Modifying test logic or assertions
- Adding new tests
- Changing integration test behavior
- Performance optimization beyond mock configuration

## Dependencies
- Existing mock helper functions in `helpers/mock-llm-api.js`
- Application server running on localhost:5000
- Playwright test framework
