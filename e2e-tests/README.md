# E2E Tests for Kabotan Halloween Features

This directory contains end-to-end tests for the Halloween-themed features using Playwright.

## Directory Structure

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
├── helpers/
│   └── mock-llm-api.js          # Mock helpers for UI tests
└── scripts/                     # Verification scripts
    ├── verify-html-structure.js
    ├── verify-mock-responses.js
    └── verify-mock-edge-cases.js
```

## Test Categories

### UI Tests (`e2e-tests/ui/`)

**Purpose**: Verify user interface behavior and interactions without requiring real LLM API calls.

**Characteristics**:
- Use mocks for all LLM API responses
- Fast execution (< 60 seconds total)
- Test UI elements, form validation, navigation
- Test error handling with mocked errors
- Test language switching UI behavior
- No API key required

**When to add UI tests**:
- Testing form validation and input handling
- Testing UI element visibility and interactions
- Testing navigation and tab switching
- Testing error message display
- Testing loading indicators
- Testing HTML structure and styling
- Testing client-side JavaScript functionality

**Example UI test**:
```javascript
test('should display monster diagnostic form', async ({ page }) => {
  await page.goto('/');
  await page.click('button[data-feature="monster-diagnostic"]');
  
  // Verify form elements
  await expect(page.locator('textarea[name="symptoms"]')).toBeVisible();
  await expect(page.locator('button[type="submit"]')).toBeVisible();
});
```

### Integration Tests (`e2e-tests/integration/`)

**Purpose**: Verify end-to-end functionality with real LLM API calls.

**Characteristics**:
- No mocks - use real OpenAI-compatible endpoint
- Slow execution (up to 180 seconds per test)
- Test actual LLM responses
- Test streaming functionality with real SSE
- Test multilingual support with real translations
- Requires `OPENAI_API_KEY` environment variable

**When to add integration tests**:
- Testing complete user workflows with real API
- Verifying LLM response quality and format
- Testing streaming content delivery
- Testing multilingual support with real translations
- Verifying API integration and error handling
- Testing session management and conversation history

**Example integration test**:
```javascript
test('should generate monster diagnostic with real LLM', async ({ page }) => {
  await page.goto('/');
  await page.click('button[data-feature="monster-diagnostic"]');
  
  // Fill form and submit
  await page.fill('textarea[name="symptoms"]', 'I turn into a bat at night');
  await page.click('button[type="submit"]');
  
  // Wait for real LLM response (may take up to 180 seconds)
  await page.waitForSelector('.diagnostic-result', { timeout: 180000 });
  
  // Verify response content
  const result = await page.locator('.diagnostic-result').textContent();
  expect(result.length).toBeGreaterThan(100);
});
```

## Prerequisites

- Node.js (v16 or higher)
- npm or yarn
- Running Kabotan application (or configured to auto-start via playwright.config.js)
- For integration tests: Valid `OPENAI_API_KEY` environment variable

## Installation

Install Playwright and dependencies:

```bash
npm install
npx playwright install
```

## Running Tests

### Quick Start (Recommended)

#### Using Makefile Commands

**UI Tests (< 60 seconds)** - Fast tests with mocks for development
```bash
make test-e2e-ui
```
Runs all UI tests in `e2e-tests/ui/` directory. No API key required.

**Integration Tests (up to 30 minutes)** - Comprehensive tests with real LLM API
```bash
make test-e2e-integration
```
Runs all integration tests in `e2e-tests/integration/` directory. Requires `OPENAI_API_KEY` environment variable.

**All E2E Tests** - Run both UI and integration tests
```bash
make test-e2e-all
```

#### Using npx Commands Directly

**UI Tests Only (< 60 seconds)**
```bash
npx playwright test --project=ui-tests --reporter=list --quiet
```

**Integration Tests Only (up to 30 minutes)**
```bash
# Set environment variable first
export OPENAI_API_KEY=your_api_key

# Run integration tests
npx playwright test --project=integration-tests --reporter=list --quiet
```

**All Tests**
```bash
npx playwright test --reporter=list --quiet
```

**Run specific test file**
```bash
# UI test
npx playwright test e2e-tests/ui/monster-diagnostic.spec.js

# Integration test
npx playwright test e2e-tests/integration/monster-diagnostic-integration.spec.js
```

**Run tests in headed mode (see browser)**
```bash
npx playwright test --project=ui-tests --headed
```

**Run tests in debug mode**
```bash
npx playwright test --project=ui-tests --debug
```

## Test Execution Time

| Test Category | Location | Execution Time | API Key Required |
|--------------|----------|----------------|------------------|
| UI Tests | `e2e-tests/ui/` | < 60 seconds | No |
| Integration Tests | `e2e-tests/integration/` | Up to 30 minutes | Yes |
| All Tests | Both directories | Up to 30 minutes | Yes (for integration) |

### UI Tests Performance
- Fast execution with mocked LLM responses
- Suitable for development and CI/CD pipelines
- High success rate (98%+)
- No external dependencies

### Integration Tests Performance
- Slow execution with real LLM API calls
- Each test may take up to 180 seconds
- Execution time varies based on LLM response time
- May fail due to API rate limits or network issues
- Recommended for pre-release validation and nightly builds

## Test Files

### UI Tests (`e2e-tests/ui/`)

UI tests verify user interface behavior with mocked LLM responses. No API key required.

#### Feature-Specific UI Tests
- `character-chat.spec.js` - Character chat UI, form validation, character selection
- `monster-diagnostic.spec.js` - Monster diagnostic form, input validation, result display
- `story-generator.spec.js` - Story generator form, style selection, story display
- `spell-generator.spec.js` - Spell generator UI, automatic loading, regeneration
- `trivia-bot.spec.js` - Trivia bot UI, question submission, response display

#### Cross-Feature UI Tests
- `error-handling.spec.js` - Error message display, validation errors, API error handling
- `language-switching.spec.js` - Language switching UI, form label updates, localStorage

### Integration Tests (`e2e-tests/integration/`)

Integration tests verify end-to-end functionality with real LLM API. Requires `OPENAI_API_KEY`.

#### Feature Integration Tests
- `character-chat-integration.spec.js` - Chat with Dracula/Witch, streaming, conversation history
- `monster-diagnostic-integration.spec.js` - Monster diagnostic with real LLM, English/Japanese
- `story-generator-integration.spec.js` - Story generation with different styles, streaming
- `spell-generator-integration.spec.js` - Spell generation on load, regeneration, streaming
- `trivia-bot-integration.spec.js` - Trivia Q&A, follow-up questions, context maintenance

#### Cross-Feature Integration Tests
- `streaming-integration.spec.js` - SSE connection, incremental delivery, completion detection
- `multilingual-integration.spec.js` - Language parameter transmission, English/Japanese responses

### Helper Modules
- `helpers/mock-llm-api.js` - Mock helpers for UI tests (not used in integration tests)

## When to Add Tests

### Add UI Tests When:
- Adding new UI components or forms
- Implementing form validation logic
- Adding new features with user interactions
- Implementing error handling UI
- Adding language switching functionality
- Modifying HTML structure or styling
- Adding client-side JavaScript functionality

### Add Integration Tests When:
- Implementing new LLM-powered features
- Modifying API endpoints or request/response format
- Implementing streaming functionality
- Adding multilingual support
- Modifying session management or conversation history
- Changing LLM prompt templates
- Implementing new error handling for API failures

### Examples

#### Example: Adding a New Feature

When adding a new feature called "Potion Generator":

**1. Add UI Test** (`e2e-tests/ui/potion-generator.spec.js`):
```javascript
const { test, expect } = require('@playwright/test');
const { mockNonStreamingEndpoint } = require('../helpers/mock-llm-api');

test.describe('Potion Generator UI', () => {
  test.beforeEach(async ({ page }) => {
    await mockNonStreamingEndpoint(page, '/api/potion-generator');
    await page.goto('/');
  });

  test('should display potion generator form', async ({ page }) => {
    await page.click('button[data-feature="potion-generator"]');
    
    await expect(page.locator('input[name="ingredients"]')).toBeVisible();
    await expect(page.locator('select[name="effect"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test('should validate required fields', async ({ page }) => {
    await page.click('button[data-feature="potion-generator"]');
    await page.click('button[type="submit"]');
    
    // HTML5 validation should prevent submission
    const isValid = await page.evaluate(() => {
      const form = document.querySelector('form');
      return form.checkValidity();
    });
    expect(isValid).toBe(false);
  });
});
```

**2. Add Integration Test** (`e2e-tests/integration/potion-generator-integration.spec.js`):
```javascript
const { test, expect } = require('@playwright/test');

test.describe('Potion Generator Integration', () => {
  test('should generate potion with real LLM', async ({ page }) => {
    await page.goto('/');
    await page.click('button[data-feature="potion-generator"]');
    
    await page.fill('input[name="ingredients"]', 'eye of newt, wing of bat');
    await page.selectOption('select[name="effect"]', 'healing');
    await page.click('button[type="submit"]');
    
    // Wait for real LLM response
    await page.waitForSelector('.potion-result', { timeout: 180000 });
    
    const result = await page.locator('.potion-result').textContent();
    expect(result.length).toBeGreaterThan(50);
    expect(result).toContain('potion');
  });

  test('should generate potion in Japanese', async ({ page }) => {
    await page.goto('/');
    await page.click('button[data-lang="ja"]');
    await page.click('button[data-feature="potion-generator"]');
    
    await page.fill('input[name="ingredients"]', 'イモリの目、コウモリの翼');
    await page.selectOption('select[name="effect"]', 'healing');
    await page.click('button[type="submit"]');
    
    await page.waitForSelector('.potion-result', { timeout: 180000 });
    
    const result = await page.locator('.potion-result').textContent();
    expect(result).toMatch(/[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/); // Contains Japanese
  });
});
```

## Environment Setup for Integration Tests

Integration tests require the following environment variables:

```bash
# Required: OpenAI-compatible API key
export OPENAI_API_KEY=your_api_key

# Optional: Custom API endpoint (default: configured in backend)
export OPENAI_HOST=http://your-api-host/v1/chat/completions

# Optional: Custom model name (default: configured in backend)
export OPENAI_MODEL=your-model-name
```

### Setting Up Environment Variables

**For local development**:
```bash
# Add to your shell profile (~/.bashrc, ~/.zshrc, etc.)
export OPENAI_API_KEY=your_api_key
```

**For CI/CD (GitLab)**:
1. Navigate to **Settings > CI/CD > Variables**
2. Add `OPENAI_API_KEY` as a masked, protected variable

**For CI/CD (GitHub Actions)**:
1. Navigate to **Settings > Secrets and variables > Actions**
2. Add `OPENAI_API_KEY` as a repository secret

## Integration Test Success Criteria

Integration tests are considered successful when:
- API response is received within 180 seconds
- Response has expected HTML structure
- Response is correctly displayed in UI
- No errors occur during execution

## Known Issues with Integration Tests

- **API Rate Limits**: May hit rate limits during consecutive runs
- **Network Timeouts**: May exceed 180 seconds depending on network conditions
- **LLM Variability**: LLM response content varies between runs
- **API Availability**: Tests fail if API endpoint is unreachable

## Troubleshooting Integration Tests

If integration tests fail:

1. **Verify API key**: Ensure `OPENAI_API_KEY` is correctly set
2. **Check API endpoint**: Verify endpoint is reachable from test environment
3. **Check network**: Ensure stable network connection
4. **Check rate limits**: Verify you haven't hit API rate limits
5. **Increase timeout**: Try `--timeout=300000` (5 minutes) if needed
6. **Check logs**: Review test output and application logs for errors

## Test Coverage

### UI Tests Coverage

#### Monster Diagnostic
- Form display and validation
- Empty field validation
- Result display with mocked response
- Japanese language support

#### Story Generator
- Form display with style options
- Gothic, Parody, and Classic style generation
- Empty field validation
- Story display formatting with mocked response

#### Character Chat
- Character selection (Dracula, Witch, Jack-o'-Lantern)
- Message submission with mocked response
- Empty message validation
- UI state management

#### Trivia Bot
- Message submission with mocked response
- Empty message validation
- UI state management

#### Spell Generator
- Automatic spell loading with mocked response
- Spell phrase and explanation display
- Japanese language support

#### Language Switching
- English to Japanese switching
- Japanese to English switching
- Feature tab text updates
- localStorage persistence
- Form label updates

#### Error Handling
- Invalid language parameter handling
- Missing required field validation
- API error response handling (mocked)
- Empty API response handling (mocked)
- Japanese error messages
- Application stability after errors

### Integration Tests Coverage

#### Monster Diagnostic Integration
- Real LLM API call with English input
- Real LLM API call with Japanese input
- Streaming response delivery
- Result format and content validation

#### Story Generator Integration
- Real LLM API call with different styles
- English and Japanese story generation
- Streaming content delivery
- Story length and format validation

#### Character Chat Integration
- Chat with Dracula character (English)
- Chat with Witch character (Japanese)
- Streaming content delivery
- Conversation history persistence

#### Spell Generator Integration
- Spell generation on load with real LLM
- Spell regeneration with real LLM
- Streaming content delivery
- English and Japanese spell generation

#### Trivia Bot Integration
- Trivia question answering with real LLM
- Follow-up questions with context
- Streaming responses
- English and Japanese trivia

#### Streaming Integration
- SSE connection establishment
- Incremental content delivery verification
- Streaming completion detection
- Final content integrity validation

#### Multilingual Integration
- Language parameter transmission to API
- English response validation from real LLM
- Japanese response validation from real LLM
- Language switching during session

## Configuration

The test configuration is in `playwright.config.js` at the project root.

### Projects Configuration

```javascript
projects: [
  {
    name: 'ui-tests',
    testDir: './e2e-tests/ui',
    timeout: 30000, // 30 seconds per test
  },
  {
    name: 'integration-tests',
    testDir: './e2e-tests/integration',
    timeout: 180000, // 180 seconds per test
  },
]
```

### Key Settings

- **Base URL**: `http://localhost:5000`
- **Test directories**: `./e2e-tests/ui` and `./e2e-tests/integration`
- **Auto-start server**: `make run`
- **Browser**: Chromium (Desktop Chrome)
- **Retries**: 2 (in CI), 0 (local)
- **Workers**: 1 (sequential execution)
- **UI test timeout**: 30 seconds
- **Integration test timeout**: 180 seconds

### Notes

- Tests run sequentially (workers: 1) to avoid race conditions
- Application server starts automatically before tests run
- Screenshots captured on test failures
- Traces recorded on first retry for debugging
- Different timeouts for UI tests (30s) and integration tests (180s)

## Debugging Failed Tests

### View test report
```bash
npx playwright show-report
```

### Run specific test in debug mode
```bash
# UI test
npx playwright test e2e-tests/ui/monster-diagnostic.spec.js --debug

# Integration test
npx playwright test e2e-tests/integration/monster-diagnostic-integration.spec.js --debug
```

### Run tests in headed mode
```bash
# UI tests
npx playwright test --project=ui-tests --headed

# Integration tests
npx playwright test --project=integration-tests --headed
```

### View traces
Traces are automatically captured on first retry. View them in the HTML report.

### Common Issues

**UI tests fail with "Mock not set up"**:
- Ensure `mockNonStreamingEndpoint` or `mockStreamingEndpoint` is called in `beforeEach`
- Verify mock helper is imported: `const { mockNonStreamingEndpoint } = require('../helpers/mock-llm-api');`

**Integration tests timeout**:
- Verify `OPENAI_API_KEY` is set correctly
- Check API endpoint is reachable
- Increase timeout if needed: `--timeout=300000`

**Tests fail intermittently**:
- Check for race conditions in test code
- Verify selectors are stable and unique
- Add explicit waits for dynamic content

## CI/CD Integration

### Recommended CI/CD Setup

#### Pull Request Pipeline (Fast Feedback)

**Purpose**: Provide quick feedback on PRs without blocking development

**Configuration** (GitLab CI):
```yaml
test:e2e:pr:
  stage: test
  image: mcr.microsoft.com/playwright:v1.40.0-jammy
  before_script:
    - npm ci
  script:
    - npx playwright test --project=ui-tests --reporter=list --quiet
  only:
    - merge_requests
  artifacts:
    when: on_failure
    paths:
      - test-results/
      - playwright-report/
    expire_in: 7 days
```

**Characteristics**:
- Execution time: < 60 seconds
- Success rate: 98%+
- No LLM API key required
- Runs only UI tests
- Ideal for PR validation

#### Main Branch Pipeline (Comprehensive Validation)

**Purpose**: Run all UI tests after merging to main branch

**Configuration** (GitLab CI):
```yaml
test:e2e:main:
  stage: test
  image: mcr.microsoft.com/playwright:v1.40.0-jammy
  before_script:
    - npm ci
  script:
    - npx playwright test --project=ui-tests --reporter=list --quiet
  only:
    - main
  artifacts:
    when: on_failure
    paths:
      - test-results/
      - playwright-report/
    expire_in: 7 days
```

**Characteristics**:
- Execution time: < 60 seconds
- Includes all UI tests
- No LLM API key required
- Comprehensive UI coverage without real API calls

#### Nightly/Release Pipeline (Full Integration)

**Purpose**: Verify real LLM API integration before releases

**Configuration** (GitLab CI):
```yaml
test:e2e:nightly:
  stage: test
  image: mcr.microsoft.com/playwright:v1.40.0-jammy
  before_script:
    - npm ci
  script:
    - npx playwright test --project=integration-tests --reporter=list --quiet
  variables:
    OPENAI_API_KEY: $OPENAI_API_KEY
    OPENAI_HOST: $OPENAI_HOST
    OPENAI_MODEL: $OPENAI_MODEL
  only:
    - schedules
    - tags
  artifacts:
    when: always
    paths:
      - test-results/
      - playwright-report/
    expire_in: 30 days
  allow_failure: true
```

**Characteristics**:
- Execution time: Up to 30 minutes
- Includes all integration tests
- Requires LLM API key
- May fail due to API rate limits or network issues
- Recommended for nightly builds and releases

### Environment Variables for CI/CD

#### Required for Integration Tests
- `OPENAI_API_KEY`: OpenAI-compatible API key

#### Optional Configuration
- `OPENAI_HOST`: Custom API endpoint (default: configured in backend)
- `OPENAI_MODEL`: Custom model name (default: configured in backend)

#### Setting Up in GitLab CI/CD

1. Navigate to **Settings > CI/CD > Variables**
2. Add the following variables:
   - `OPENAI_API_KEY`: Your API key (masked, protected)
   - `OPENAI_HOST`: Your API endpoint (optional)
   - `OPENAI_MODEL`: Your model name (optional)

#### Setting Up in GitHub Actions

1. Navigate to **Settings > Secrets and variables > Actions**
2. Add the following secrets:
   - `OPENAI_API_KEY`: Your API key
   - `OPENAI_HOST`: Your API endpoint (optional)
   - `OPENAI_MODEL`: Your model name (optional)

### Performance Comparison

| Pipeline Type | Execution Time | Tests Run | API Key Required | Use Case |
|--------------|----------------|-----------|------------------|----------|
| PR Pipeline | < 60 seconds | UI tests only | No | Quick PR validation |
| Main Pipeline | < 60 seconds | UI tests only | No | Post-merge validation |
| Nightly Pipeline | Up to 30 minutes | Integration tests | Yes | Release validation |

### Best Practices

1. **Use PR pipeline for all pull requests**: Fast feedback without API costs
2. **Run main pipeline on merge**: Comprehensive UI validation before deployment
3. **Schedule nightly pipeline**: Regular integration testing with real API
4. **Monitor success rates**: UI tests should maintain 98%+ success rate
5. **Review integration test failures**: May indicate API changes or issues

### Troubleshooting CI/CD Issues

#### Tests Fail in CI but Pass Locally
- Ensure Node.js version matches (v16+)
- Check Playwright version consistency
- Verify timeout settings are appropriate
- Review CI environment differences

#### Integration Tests Timeout
- Increase timeout in `playwright.config.js` if needed
- Check API endpoint accessibility from CI
- Verify API key is correctly configured
- Consider running integration tests less frequently

#### Flaky Tests
- Review test logs in artifacts
- Check for race conditions
- Increase wait times if needed
- Consider marking as `allow_failure: true` temporarily

## Test Organization Benefits

### Clear Separation of Concerns
- **UI tests** focus on interface behavior and user interactions
- **Integration tests** focus on end-to-end functionality with real API
- No mixing of mocked and real API calls in the same test file

### Faster Development Cycle
- Run UI tests during development (< 60 seconds)
- Run integration tests before releases (up to 30 minutes)
- No need to wait for slow integration tests during active development

### Better Test Reliability
- UI tests have high success rate (98%+) due to mocked responses
- Integration tests may fail due to external factors (API limits, network)
- Clear distinction helps identify root cause of failures

### Cost Optimization
- UI tests don't consume API credits
- Integration tests only run when necessary (nightly, releases)
- Reduced API costs while maintaining comprehensive coverage

## Migration from Old Structure

The test suite was reorganized from a mixed structure to separate UI and integration tests:

### Before (Mixed Structure)
```
e2e-tests/
├── character-chat.spec.js (mixed: UI + [INTEGRATION])
├── monster-diagnostic.spec.js (mixed: UI + [INTEGRATION])
└── ... (other mixed test files)
```

### After (Separated Structure)
```
e2e-tests/
├── ui/                          # UI tests with mocks
│   ├── character-chat.spec.js
│   └── ...
└── integration/                 # Integration tests without mocks
    ├── character-chat-integration.spec.js
    └── ...
```

### Key Changes
- Removed `[INTEGRATION]` tags from test names
- Separated UI tests into `ui/` directory
- Separated integration tests into `integration/` directory
- Removed mock imports from integration tests
- Added separate Playwright projects for each test category
- Different timeouts for UI (30s) and integration (180s) tests
