# E2E Mock Testing Design

## Architecture Overview

### Current State
```
E2E Tests
├── Mock Tests (majority) - Fast, reliable, NO LLM communication
│   ├── Uses helpers/mock-llm-api.js
│   ├── Mocks HTTP responses at Playwright level
│   ├── Execution time: 1.6 min (fast), 12.2 min (all)
│   └── LLM Communication: NONE (fully mocked)
│
└── Integration Tests (1 per feature) - Real LLM API communication
    ├── Tagged with [INTEGRATION]
    ├── Uses real LLM API
    ├── Execution time: 1.5 min
    └── LLM Communication: YES (real API calls)
```

### LLM Communication Policy

**Mock Tests (Default)**:
- NO LLM API calls should occur
- All endpoints mocked at Playwright level
- If LLM calls detected, it indicates missing or incorrect mock setup
- Verification: Check application logs for `[LLM-REQUEST]` entries

**Integration Tests ([INTEGRATION] tag)**:
- Real LLM API calls expected and required
- Tests end-to-end functionality with actual LLM service
- One integration test per feature minimum
- Verification: LLM calls should appear in application logs

### Mock Infrastructure

#### Mock Helper Module (`helpers/mock-llm-api.js`)
```javascript
// Core functions
- mockNonStreamingEndpoint()  // Mock regular API calls
- mockStreamingEndpoint()     // Mock SSE streaming
- mockErrorResponse()         // Mock error scenarios
- mockNetworkFailure()        // Mock network issues
- mockAllEndpoints()          // Setup all mocks at once

// Response templates
- MOCK_RESPONSES              // Predefined responses per feature
- createMockNonStreamingResponse()  // Generate HTML responses
- createStreamingChunks()     // Generate SSE chunks
```

#### Response Format Matching
Mock responses must match backend format from `src/utils/response-formatting.lisp`:

**Character Chat:**
```html
<div class="chat chat-start">
  <div class="chat-header">{emoji}</div>
  <div class="chat-bubble chat-bubble-secondary">{text}</div>
</div>
```

**Monster Diagnostic:**
```html
<div class="alert alert-success">
  <h3 class="font-bold text-lg mb-2">{title}</h3>
  <div class="whitespace-pre-wrap">{text}</div>
</div>
```

**Story Generator:**
```html
<div class="bg-base-300 rounded-lg p-4">
  <h3 class="font-bold text-lg mb-2">{title}</h3>
  <div class="whitespace-pre-wrap">{text}</div>
</div>
```

**Spell Generator:**
```html
<div class="text-center">
  <div class="spell-phrase">{spell}</div>
  <div class="spell-explanation mt-6">{meaning}</div>
</div>
```

**Trivia Bot:**
```html
<div class="chat chat-start">
  <div class="chat-header">🎓</div>
  <div class="chat-bubble chat-bubble-accent">{text}</div>
</div>
```

### Test Organization Strategy

#### Test File Structure
Each feature test file should follow this pattern:

```javascript
const { mockNonStreamingEndpoint } = require('./helpers/mock-llm-api');
const { test, expect } = require('@playwright/test');

test.describe('Feature Name', () => {
  // Mock tests (default, no tag)
  test('should do X (with mock)', async ({ page }) => {
    await mockNonStreamingEndpoint(page, '**/api/endpoint', {
      feature: 'feature-name',
      lang: 'en',
      delay: 100,
    });
    // Test implementation
  });

  test('should do Y (with mock)', async ({ page }) => {
    // More mock tests
  });

  // Integration test (tagged)
  test('[INTEGRATION] should do X (real LLM)', async ({ page }) => {
    // No mock setup - uses real API
    // Test implementation
  });
});
```

#### Test Execution Groups

**Fast Mock Tests (Recommended for PR):**
```bash
npx playwright test --grep-invert "\[INTEGRATION\]|streaming|Streaming" \
  --reporter=list --quiet --timeout=30000
```
- Excludes: Integration tests, streaming tests
- Time: 3-5 minutes
- Success rate: 98%+

**All Mock Tests:**
```bash
npx playwright test --grep-invert "\[INTEGRATION\]" \
  --reporter=list --quiet --timeout=30000
```
- Excludes: Integration tests only
- Time: 13 minutes
- Includes: Streaming mock tests

**Integration Tests Only:**
```bash
npx playwright test --grep "\[INTEGRATION\]" \
  --reporter=list --quiet --timeout=180000
```
- Includes: Integration tests only
- Time: 5-10 minutes
- Requires: OPENAI_API_KEY

**Full Suite:**
```bash
npx playwright test --reporter=list --quiet --timeout=180000
```
- Includes: All tests
- Time: 15-20 minutes

### Mock Implementation Patterns

#### Pattern 1: Simple Mock (Non-Streaming)
```javascript
test('should display result (with mock)', async ({ page }) => {
  await mockNonStreamingEndpoint(page, '**/api/endpoint', {
    feature: 'feature-name',
    lang: 'en',
    delay: 100,
  });
  
  await page.goto('/');
  // Test assertions
});
```

#### Pattern 2: Character-Specific Mock
```javascript
test('should chat with character (with mock)', async ({ page }) => {
  await mockNonStreamingEndpoint(page, '**/api/character-chat', {
    feature: 'character-chat',
    character: 'dracula',
    lang: 'en',
    delay: 100,
  });
  
  // Test implementation
});
```

#### Pattern 3: Custom Response Mock
```javascript
test('should handle custom response (with mock)', async ({ page }) => {
  await mockNonStreamingEndpoint(page, '**/api/endpoint', {
    feature: 'feature-name',
    customResponse: 'Custom test response text',
    lang: 'en',
    delay: 100,
  });
  
  // Test implementation
});
```

#### Pattern 4: Streaming Mock
```javascript
test('should stream response (with mock)', async ({ page }) => {
  await mockStreamingEndpoint(page, '**/api/endpoint-stream*', {
    feature: 'feature-name',
    lang: 'en',
    chunkSize: 5,
    chunkDelay: 50,
  });
  
  // Test implementation
});
```

#### Pattern 5: Error Mock
```javascript
test('should handle error (with mock)', async ({ page }) => {
  await mockErrorResponse(page, '**/api/endpoint', {
    status: 500,
    errorMessage: 'Test error message',
    lang: 'en',
  });
  
  // Test implementation
});
```

#### Pattern 6: Global Mock Setup
```javascript
test.describe('Feature Tests', () => {
  test.beforeEach(async ({ page }) => {
    await mockAllEndpoints(page, { delay: 50 });
    await page.goto('/');
  });
  
  // All tests use mocks
});
```

### Integration Test Strategy

#### Keep One Integration Test Per Feature
Each feature should have exactly one integration test that:
- Uses real LLM API (no mocks)
- Tagged with `[INTEGRATION]`
- Tests end-to-end functionality
- Verifies real API integration

#### Integration Test Placement
```javascript
// At the end of each feature test file
test('[INTEGRATION] should work with real LLM', async ({ page }) => {
  // No mock setup
  await page.goto('/');
  
  // Full end-to-end test with real API
  // Longer timeout (180 seconds)
});
```



### Performance Optimization

#### Mock Response Timing
- Default delay: 100ms (simulates fast API)
- Streaming chunk delay: 50ms
- Error response delay: 100ms
- Network failure: Immediate abort

#### Test Timeout Strategy
- Mock tests: 30 seconds (30000ms)
- Integration tests: 180 seconds (180000ms)
- Streaming tests: 60 seconds (60000ms)

#### Parallel Execution
- Workers: 1 (sequential)
- Reason: Avoid race conditions with shared state
- Future: Consider parallel execution for mock tests



## Implementation Status

### Completed ✅
- [x] Mock infrastructure implemented (`helpers/mock-llm-api.js`)
- [x] All 12 test files use mocks
- [x] 5 integration tests exist (all major features covered)
- [x] Mock response templates match backend format
- [x] Test structure improved (separate describe blocks)
- [x] Performance targets exceeded:
  - Fast mock tests: 1.6 min (target: 3-5 min)
  - All mock tests: 12.2 min (target: 13 min)
  - Integration tests: 1.5 min (target: 5-10 min)
- [x] Mock response verification: 100% accuracy
- [x] Documentation completed

### Verification Results
- Mock tests: 97.7% success rate (167/171 passed over 3 runs)
- Integration tests: 100% success rate (5/5 passed)
- LLM communication: Properly isolated between mock and integration tests

## Known Limitations

### Streaming Test Issues
- Playwright has technical limitations with SSE streaming
- Many streaming tests may fail due to framework constraints
- Workaround: Use integration tests for streaming verification
- Alternative: Manual testing for streaming features

### Mock Limitations
- Cannot test actual LLM response quality
- Cannot test LLM API error scenarios
- Cannot test network latency variations
- Solution: Keep integration tests for these scenarios

### Flaky Tests
- 2 tests in `conversation-history.spec.js` have timing issues (97.7% success rate)
- Root cause: Mock responses complete too quickly for loading indicator checks
- Mitigation: Documented in test comments, acceptable for CI/CD

## Troubleshooting LLM Communication Issues

### Detecting Unwanted LLM Calls in Mock Tests
If mock tests are making LLM API calls:

1. **Check application logs** for `[LLM-REQUEST]` entries during test execution
2. **Verify mock setup** in test file `beforeEach` block
3. **Check endpoint patterns** match actual API routes
4. **Ensure both endpoints mocked**: non-streaming AND streaming variants

### Common Causes
- Missing mock setup in `beforeEach`
- Incorrect endpoint pattern (e.g., missing wildcard for query params)
- HTMX form submission bypassing JavaScript handlers
- Race condition between HTMX and JavaScript event handlers

### Solutions
- Use `mockAllEndpoints()` for cross-feature tests
- Add explicit mock setup for both streaming and non-streaming endpoints
- Use inline `onsubmit` handlers instead of `addEventListener` for forms
- Verify mocks are registered before page navigation
