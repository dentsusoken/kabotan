# Development Guidelines

## Git Commands
When using git commands, always use the `--no-pager` option to prevent the pager from starting.

### Examples
```bash
git --no-pager status
git --no-pager log
git --no-pager diff
git --no-pager show
```

This ensures that git commands complete immediately without waiting for user interaction with a pager.

## Application Execution
When running the application for testing or development purposes, always use **background execution** with the `controlBashProcess` tool.

### Background Execution Rules
- NEVER use `executeBash` for long-running commands like `make run` or `make repl`
- ALWAYS use `controlBashProcess` with action "start" for application servers
- Use `getProcessOutput` to check if the application started successfully
- Use `controlBashProcess` with action "stop" to terminate the process when done

### Examples
```bash
# Start application in background
controlBashProcess(action: "start", command: "make run")

# Check application output
getProcessOutput(processId: <id>)

# Stop application
controlBashProcess(action: "stop", processId: <id>)
```

This ensures that the application runs without blocking other operations.

## Roswell Script Execution
When executing Roswell scripts or running Common Lisp code directly, always use the `--disable-debugger` flag to prevent the debugger from starting on errors.

### Debugger Disabling Rules
- ALWAYS use `--disable-debugger` flag when running `ros` commands
- This prevents the REPL debugger from blocking execution
- Errors will be printed to stderr and the process will exit with non-zero status

### Examples
```bash
# Run a Roswell script with debugger disabled
ros run -- --disable-debugger script.lisp

# Execute Lisp code with debugger disabled
ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --eval '(kabotan:main)'

# Load and run tests with debugger disabled
ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --eval '(asdf:test-system :kabotan)'

# Quick load check (compile only)
ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit
```

This ensures that scripts complete execution without waiting for user interaction in the debugger.

## Testing Strategy

This project uses a multi-layered testing approach:

### Backend Unit Tests (FiveAM)
- Test files located in `/tests/` directory
- Test package: `:kabotan.tests`
- Run tests with: `make test`

#### Test Organization
- `tests.lisp`: Main test runner and basic functionality tests
- `message-api-tests.lisp`: Message API and validation tests
- `logging-tests.lisp`: Logging functionality tests
- `streaming-llm-test.lisp`: Streaming LLM integration tests
- `manual-streaming-test.lisp`: Manual streaming tests (for development)
- `manual-streaming-llm-test.lisp`: Manual LLM streaming tests (for development)

#### Test Guidelines
- Each test suite should be clearly defined with `defsuite`
- Use descriptive test names that explain what is being tested
- Tests should be independent and not rely on execution order
- Mock external dependencies (e.g., LLM API calls) when appropriate
- All new functions should have corresponding tests
- Edge cases and error conditions should be tested

### E2E Tests (Playwright)
- Test files located in `/e2e-tests/` directory
- Configuration in `playwright.config.js`
- Run with npm scripts defined in `package.json`

#### E2E Test Organization
- Feature-specific test files: `<feature-name>.spec.js`
- Streaming-specific tests: `streaming-<feature-name>.spec.js`
- Conversation history tests: `conversation-history.spec.js`
- README.md with detailed testing documentation

#### Test Execution Commands
```bash
# Run all non-streaming E2E tests
npm run test:e2e

# Run only streaming E2E tests
npm run test:e2e:streaming

# Run all E2E tests (including streaming)
npm run test:e2e:all

# Run with headed browser
npm run test:e2e:headed

# Debug mode
npm run test:e2e:debug
```

### Test Coverage Expectations
- Backend: All utility functions, services, and API handlers
- Frontend: All features with both streaming and non-streaming modes
- Integration: All API endpoints with real LLM responses
- E2E: Complete user workflows for all features

## E2E Testing with Playwright

### E2E Test Execution
When running E2E tests, use the following guidelines:

#### Recommended Approach: Use npm scripts
```bash
# Run non-streaming tests (faster, more reliable)
npm run test:e2e

# Run streaming tests (requires real LLM responses)
npm run test:e2e:streaming

# Run all tests
npm run test:e2e:all

# Run with headed browser for debugging
npm run test:e2e:headed

# Debug mode with Playwright Inspector
npm run test:e2e:debug
```

#### Direct Playwright Commands
When running tests directly with `npx playwright test`:

- ALWAYS use minimal output mode to show only failed test errors
- ALWAYS use `-x` flag to stop on first failure for faster feedback
- Do NOT use HTML report mode (avoid `--reporter=html`)
- Do NOT use line reporter (avoid `--reporter=line`)
- Use list reporter with quiet mode: `--reporter=list --quiet -x`
- Timeout is configured in `playwright.config.js` (360 seconds)
- For full test runs, use: `npx playwright test --reporter=list --quiet -x`
- For specific test files: `npx playwright test <file> --reporter=list --quiet -x`
- For specific tests: `npx playwright test --grep "<test name>" --reporter=list --quiet -x`

### Examples
```bash
# Run all E2E tests with minimal output (stop on first failure)
npx playwright test --reporter=list --quiet -x

# Run specific test file (stop on first failure)
npx playwright test e2e-tests/character-chat.spec.js --reporter=list --quiet -x

# Run specific test by name (stop on first failure)
npx playwright test --grep "should chat with Dracula" --reporter=list --quiet -x

# Run only streaming tests (stop on first failure)
npx playwright test --grep "streaming|Streaming" --reporter=list --quiet -x

# Run only non-streaming tests (stop on first failure)
npx playwright test --grep-invert "streaming|Streaming" --reporter=list --quiet -x
```

### Test Configuration
- **Timeout**: 360 seconds (6 minutes) per test - configured in `playwright.config.js`
- **Workers**: 1 (sequential execution to avoid race conditions)
- **Retries**: 2 in CI, 0 locally
- **Base URL**: http://localhost:5000
- **Browser**: Chromium with cache disabled
- **Web Server**: Automatically started with `make run`

### Notes
- E2E tests require the application to be running on port 5000
- Tests run sequentially (workers: 1) to avoid race conditions
- LLM responses may take up to 5 minutes in some cases
- Streaming tests are separated from non-streaming tests for faster CI
- The list reporter with quiet mode shows only failed test error details
- Screenshots are captured only on failure
- Traces are captured on first retry for debugging
