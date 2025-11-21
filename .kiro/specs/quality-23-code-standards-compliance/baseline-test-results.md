# Baseline Test Results

**Date:** 2024-11-18
**Branch:** main (before refactoring)
**Backup Branch:** backup/pre-code-standards-refactoring

## Test Suite Summary

- **Total Checks:** 799
- **Passed:** 779 (97%)
- **Failed:** 20 (2%)
- **Skipped:** 0 (0%)

## Test Suites

1. **VALIDATION-SUITE** - ✅ All tests passing
2. **FEATURE-CONTENT-SUITE** - ✅ All tests passing
3. **ERROR-HANDLING-SUITE** - ✅ All tests passing
4. **RESPONSE-FORMATTING-SUITE** - ✅ All tests passing
5. **LANGUAGE-HANDLER-SUITE** - ✅ All tests passing
6. **PROMPT-BUILDER-SUITE** - ✅ All tests passing
7. **LLM-SERVICE-SUITE** - ✅ All tests passing
8. **STREAMING-SUITE** - ✅ All tests passing
9. **API-ENDPOINT-SUITE** - ⚠️ 2 failures
10. **STREAMING-HANDLER-SUITE** - ⚠️ 17 failures
11. **LOGGING-SUITE** - ✅ All tests passing
12. **MESSAGE-API-SUITE** - ✅ All tests passing
13. **HTML-TEMPLATES-SUITE** - ✅ All tests passing
14. **SESSION-MANAGER-SUITE** - ✅ All tests passing

## Known Failures (Pre-existing)

### API-ENDPOINT-SUITE (2 failures)
- `MONSTER-DIAGNOSTIC-VALID-REQUEST` - Returns 400 instead of 200
- `MONSTER-DIAGNOSTIC-LLM-FAILURE` - Returns 400 instead of 500

### STREAMING-HANDLER-SUITE (17 failures)
- `MONSTER-DIAGNOSTIC-STREAMING-VALID-REQUEST` - Returns 400 error instead of streaming response
- `MONSTER-DIAGNOSTIC-STREAMING-LLM-ERROR` - Returns 400 error instead of streaming response
- `TRIVIA-BOT-STREAMING-VALID-REQUEST` - Returns 400 error instead of streaming response
- `TRIVIA-BOT-STREAMING-MISSING-MESSAGES` - Error message mismatch
- `TRIVIA-BOT-STREAMING-LLM-ERROR` - Returns 400 error instead of streaming response
- `STREAMING-HANDLERS-SSE-HEADERS` - Type error when extracting headers

## File Size Analysis

### Files Exceeding 500 Lines

1. **tests/tests.lisp** - 1274 lines
   - Contains: All test suites
   - Target: Split into separate test files by functional area

2. **src/services/llm-service.lisp** - 1138 lines
   - Contains: Core API, retry logic, streaming functionality
   - Target: Split into llm-client.lisp, llm-retry.lisp, llm-streaming.lisp

3. **src/utils/html-templates.lisp** - 1080 lines
   - Contains: All HTML generation functions
   - Target: Split into html-common.lisp, html-forms.lisp, html-features.lisp, html-chat.lisp

4. **src/utils/streaming.lisp** - 747 lines
   - Contains: SSE utilities and streaming handlers
   - Target: Split into sse-protocol.lisp, streaming-handlers.lisp

5. **tests/html-templates-tests.lisp** - 623 lines
   - Contains: All HTML template tests
   - Target: Split into html-common-tests.lisp, html-forms-tests.lisp, html-features-tests.lisp

## Notes

- The 20 test failures are pre-existing and not related to the refactoring work
- These failures appear to be related to parameter validation in streaming handlers
- All core functionality tests are passing
- The refactoring should maintain this baseline and not introduce new failures
- After refactoring, we should verify that the same 799 checks run and the same tests pass/fail

## Refactoring Goals

1. Split all files to be under 500 lines
2. Ensure all functions are under 100 lines
3. Ensure no nesting depth exceeds 6 levels
4. Maintain all existing test coverage (799 checks)
5. Ensure no new test failures are introduced
6. All 779 passing tests should continue to pass
