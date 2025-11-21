# Mock Test Verification - Design

## Approach

### Phase 1: Verification Setup
Create verification tools to detect LLM API calls during test execution.

### Phase 2: Systematic Verification
Test each category of files systematically, fixing issues as found.

### Phase 3: Documentation
Update documentation with findings and best practices.

## Verification Process

### Step 1: Prepare Verification Environment
```bash
# Terminal 1: Start application with visible logs
make run

# Terminal 2: Run tests and monitor
npx playwright test e2e-tests/FILE.spec.js --grep-invert "\[INTEGRATION\]"
```

### Step 2: Test Each Category

#### Category 1: Feature Tests
Test files individually:
1. `monster-diagnostic.spec.js` - 4 tests
2. `story-generator.spec.js` - 4 tests  
3. `character-chat.spec.js` - 5 tests
4. `spell-generator.spec.js` - 2 tests
5. `trivia-bot.spec.js` - 4 tests

**Expected**: No LLM logs, ~1-2s per test

#### Category 2: Streaming Tests
Test files individually:
1. `streaming-character-chat.spec.js` - 16 tests
2. `streaming-error-handling.spec.js` - 7 tests

**Expected**: No LLM logs, ~1-2s per test

#### Category 3: Cross-Feature Tests
Test files individually:
1. `conversation-history.spec.js` - 15 tests
2. `language-switching.spec.js` - 8 tests
3. `error-handling.spec.js` - 9 tests
4. `browser-compatibility.spec.js` - 8 tests

**Expected**: No LLM logs, ~1-2s per test

### Step 3: Fix Issues
For each file with LLM calls:
1. Identify which endpoint is being called
2. Add missing mock setup
3. Re-test to verify fix
4. Document the fix

## Mock Configuration Patterns

### Pattern 1: Feature Test with Both Endpoints
```javascript
test.beforeEach(async ({ page }) => {
  // Mock non-streaming endpoint
  await mockNonStreamingEndpoint(page, '**/api/FEATURE', {
    feature: 'FEATURE',
    lang: 'en',
    delay: 100,
  });
  
  // Mock streaming endpoint
  await mockStreamingEndpoint(page, '**/api/FEATURE-stream*', {
    feature: 'FEATURE',
    lang: 'en',
  });

  await page.goto('/');
  await page.waitForLoadState('networkidle');
});
```

### Pattern 2: Cross-Feature Test with All Endpoints
```javascript
test.beforeEach(async ({ page }) => {
  // Mock all endpoints at once
  await mockAllEndpoints(page, { delay: 50 });

  await page.goto('/');
  await page.waitForLoadState('networkidle');
});
```

### Pattern 3: Streaming Test with Custom Response
```javascript
test.beforeEach(async ({ page }) => {
  const customResponse = 'Custom test response...';
  
  await mockNonStreamingEndpoint(page, '**/api/FEATURE', {
    feature: 'FEATURE',
    customResponse,
    delay: 100,
  });
  
  await mockStreamingEndpoint(page, '**/api/FEATURE-stream*', {
    feature: 'FEATURE',
    customResponse,
  });

  await page.goto('/');
  await page.waitForLoadState('networkidle');
});
```

## Tracking Progress

### Verification Checklist
- [ ] Category 1: Feature Tests (5 files)
  - [ ] monster-diagnostic.spec.js
  - [ ] story-generator.spec.js
  - [ ] character-chat.spec.js
  - [ ] spell-generator.spec.js
  - [ ] trivia-bot.spec.js
- [ ] Category 2: Streaming Tests (2 files)
  - [ ] streaming-character-chat.spec.js
  - [ ] streaming-error-handling.spec.js
- [ ] Category 3: Cross-Feature Tests (4 files)
  - [ ] conversation-history.spec.js
  - [ ] language-switching.spec.js
  - [ ] error-handling.spec.js
  - [ ] browser-compatibility.spec.js
- [ ] Final verification: Run all mock tests together

## Expected Outcomes

### Before Fix
- LLM API calls visible in logs
- 429 rate limit errors
- Slow test execution (15-20 min)

### After Fix
- No LLM API calls in logs
- No rate limit errors
- Fast test execution (3-5 min)
- Clear separation between mock and integration tests

## Rollback Plan
If issues arise:
1. Git stash changes
2. Revert to previous working state
3. Re-analyze the problem
4. Apply fixes more carefully
