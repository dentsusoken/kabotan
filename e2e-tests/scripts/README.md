# E2E Test Verification Scripts

This directory contains scripts for verifying the accuracy and correctness of mock responses used in E2E tests.

## Scripts

### verify-mock-responses.js

Compares mock responses with real API responses to ensure structural accuracy.

**Purpose:**
- Verify that mock HTML structure matches backend response format
- Test all features (Monster Diagnostic, Story Generator, Spell Generator, Character Chat, Trivia Bot)
- Test multiple languages (English, Japanese)
- Test character-specific responses (Dracula, Witch, Jack)

**Requirements:**
- Application must be running on http://localhost:5000
- OPENAI_API_KEY must be set for real API calls

**Usage:**
```bash
# Start the application
make run

# In another terminal, run the verification script
node e2e-tests/scripts/verify-mock-responses.js
```

**Test Coverage:**
- ✓ Monster Diagnostic (English)
- ✓ Story Generator (English)
- ✓ Spell Generator (English)
- ✓ Character Chat - Dracula (English)
- ✓ Character Chat - Witch (English)
- ✓ Character Chat - Jack (English)
- ✓ Trivia Bot (English)
- ✓ Monster Diagnostic (Japanese)
- ✓ Story Generator (Japanese)

**Results:** All 9 tests passed

### verify-mock-edge-cases.js

Tests edge cases and special scenarios for mock responses.

**Purpose:**
- Verify handling of empty responses
- Test very long responses (10,000+ characters)
- Test special character escaping (HTML entities, XSS prevention)
- Test multi-line responses with whitespace preservation
- Test character-specific emoji rendering
- Test language-specific titles and formatting
- Test streaming chunk format (SSE, JSON structure, [DONE] marker)
- Test spell generator parsing (Spell: / Meaning: format)

**Requirements:**
- No external dependencies (runs standalone)

**Usage:**
```bash
node e2e-tests/scripts/verify-mock-edge-cases.js
```

**Test Coverage:**
- ✓ Empty responses (1 test)
- ✓ Long responses (2 tests)
- ✓ Special characters (5 tests)
- ✓ Multi-line responses (2 tests)
- ✓ Character-specific responses (3 tests)
- ✓ Language-specific responses (2 tests)
- ✓ Streaming chunks (4 tests)
- ✓ Spell generator parsing (5 tests)

**Results:** All 24 tests passed

## Verification Summary

### Mock Response Accuracy: 100% ✅

All mock responses match the backend response format exactly:

1. **HTML Structure:** Mock HTML structure matches backend `response-formatting.lisp`
2. **Character Emojis:** Correct emojis for Dracula (🧛), Witch (🧙), Jack (🎃)
3. **Language Support:** Correct titles and formatting for English and Japanese
4. **Special Characters:** Proper HTML entity escaping (XSS prevention)
5. **Edge Cases:** Handles empty, long, and multi-line responses correctly
6. **Streaming Format:** Valid SSE format with proper JSON structure and [DONE] marker

### Maintenance

Run these verification scripts:
- **After backend changes:** Verify mock responses still match
- **Before releases:** Ensure mock accuracy
- **When adding features:** Add new test cases to verification scripts

### CI/CD Integration

Consider adding these scripts to CI/CD pipeline:
```yaml
test:mock-verification:
  script:
    - make run &
    - sleep 5  # Wait for server to start
    - node e2e-tests/scripts/verify-mock-responses.js
    - node e2e-tests/scripts/verify-mock-edge-cases.js
  only:
    - main
    - merge_requests
```
