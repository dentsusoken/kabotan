# Unused Functions Analysis

## Analysis Date
2024-11-18

## Methodology
1. Extract all exported functions from `src/package.lisp`
2. Search for usage of each function across the codebase (*.lisp files)
3. Identify functions with zero call sites
4. Manually review candidates to confirm they're truly unused

## Exported Functions Analysis

### Validation Utilities

#### ✅ validate-language
- **Status**: IN USE
- **Location**: src/utils/validation.lisp
- **Usage**: Used in handler-utils.lisp (detect-language-from-request, line 127)

#### ✅ validate-non-empty-string
- **Status**: IN USE
- **Location**: src/utils/validation.lisp
- **Usage**: Used in handler-utils.lisp (validate-required-params, line 193)

#### ❌ validate-story-style
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/validation.lisp
- **Definition**: Lines 13-15
- **Search Results**: No usage found in codebase
- **Notes**: Validation function for story styles. Not used in current handlers.

#### ❌ validate-character
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/validation.lisp
- **Definition**: Lines 17-19
- **Search Results**: No usage found in codebase
- **Notes**: Validation function for character personas. Not used in current handlers.

#### ❌ sanitize-input
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/validation.lisp
- **Definition**: Lines 21-28
- **Search Results**: No usage found in codebase
- **Notes**: Input sanitization function. Not used in current handlers.

#### ❌ validate-messages-json
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/validation.lisp
- **Definition**: Lines 48-68
- **Search Results**: No usage found in codebase
- **Notes**: Legacy function from REST API era. Character chat handler uses inline JSON parsing instead.

#### ❌ validate-required-fields
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/validation.lisp
- **Definition**: Lines 70-96
- **Search Results**: No usage found in codebase
- **Notes**: All handlers use simpler `validate-required-params` from handler-utils.lisp instead.

### Error Handling Utilities

#### ❌ handle-api-error
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/error-handling.lisp
- **Definition**: Lines 8-13
- **Search Results**: No usage found in codebase
- **Notes**: Legacy function that returns property list format. Current error handling uses HTML responses.

#### ✅ format-error-response
- **Status**: IN USE
- **Location**: src/utils/error-handling.lisp
- **Usage**: Used in handler-utils.lisp (build-validation-error-response, build-service-error-response)

#### ✅ log-error
- **Status**: IN USE
- **Location**: src/utils/error-handling.lisp
- **Usage**: Used in multiple handlers and utilities

#### ✅ *suppress-error-logs*
- **Status**: IN USE
- **Location**: src/utils/error-handling.lisp
- **Usage**: Used in tests and error handling functions

### Response Formatting Utilities

#### ✅ format-html-response
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in multiple handlers

#### ✅ format-monster-diagnostic-response
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in monster-diagnostic-handler.lisp

#### ✅ format-story-response
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in story-generator-handler.lisp

#### ❌ format-character-chat-response
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/response-formatting.lisp
- **Definition**: Lines 28-40
- **Search Results**: No usage found in codebase
- **Notes**: Legacy single-message formatter. Replaced by format-character-chat-response-with-history.

#### ✅ format-character-chat-response-with-history
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in character-chat-handler.lisp

#### ❌ format-trivia-response
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/response-formatting.lisp
- **Definition**: Lines 72-78
- **Search Results**: No usage found in codebase
- **Notes**: Legacy single-message formatter. Replaced by format-trivia-response-with-history.

#### ✅ format-trivia-response-with-history
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in trivia-bot-handler.lisp

#### ✅ format-spell-generator-response
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used in spell-generator-handler.lisp

#### ✅ escape-html
- **Status**: IN USE
- **Location**: src/utils/response-formatting.lisp
- **Usage**: Used extensively in all formatting functions

### HTML Template Generation Utilities

#### ❌ escape-html-char
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 11-18
- **Search Results**: No usage found in codebase
- **Notes**: Helper function for character-level HTML escaping. Only used internally by escape-html-attribute and escape-html.

#### ❌ escape-html-attribute
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 20-25
- **Search Results**: No usage found in codebase
- **Notes**: HTML attribute escaping function. Not used in current codebase.

#### ❌ sanitize-html-id
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 33-39
- **Search Results**: No usage found in codebase
- **Notes**: HTML ID sanitization function. Not used in current codebase.

#### ❌ generate-card
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 45-60
- **Search Results**: No usage found in codebase
- **Notes**: DaisyUI card component generator. Not used in current handlers.

#### ❌ generate-alert
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 62-82
- **Search Results**: No usage found in codebase
- **Notes**: DaisyUI alert component generator. Not used in current handlers.

#### ❌ generate-loading-indicator
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 84-96
- **Search Results**: No usage found in codebase
- **Notes**: Loading indicator generator. Not used in current handlers.

#### ❌ generate-container
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 98-109
- **Search Results**: No usage found in codebase
- **Notes**: Generic container generator. Not used in current handlers.

#### ❌ generate-error-display
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 111-120
- **Search Results**: No usage found in codebase
- **Notes**: Error display container generator. Not used in current handlers.

#### ❌ generate-result-container
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-common.lisp
- **Definition**: Lines 122-130
- **Search Results**: No usage found in codebase
- **Notes**: Result container generator. Not used in current handlers.

#### ❌ generate-form-input
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Form input generator. Not used in current handlers.

#### ❌ generate-form-textarea
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Form textarea generator. Not used in current handlers.

#### ❌ generate-form-select
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Form select generator. Not used in current handlers.

#### ❌ generate-form-radio-group
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Form radio group generator. Not used in current handlers.

#### ❌ generate-button
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Button generator. Not used in current handlers.

#### ❌ generate-form-wrapper
- **Status**: UNUSED - CANDIDATE FOR REMOVAL
- **Location**: src/utils/html-forms.lisp
- **Search Results**: No usage found in codebase
- **Notes**: Form wrapper generator. Not used in current handlers.

## Summary of Unused Functions

### Confirmed Unused Functions (24 total)

#### Validation Functions (5)
1. **validate-messages-json** (src/utils/validation.lisp)
   - Legacy JSON validation from REST API era
   - No call sites found

2. **validate-required-fields** (src/utils/validation.lisp)
   - Complex field validation with custom validators
   - Replaced by simpler validate-required-params

3. **validate-story-style** (src/utils/validation.lisp)
   - Story style validation
   - No call sites found

4. **validate-character** (src/utils/validation.lisp)
   - Character persona validation
   - No call sites found

5. **sanitize-input** (src/utils/validation.lisp)
   - Input sanitization
   - No call sites found

#### Error Handling Functions (1)
6. **handle-api-error** (src/utils/error-handling.lisp)
   - Legacy error handler returning property lists
   - No call sites found

#### Response Formatting Functions (2)
7. **format-character-chat-response** (src/utils/response-formatting.lisp)
   - Legacy single-message chat formatter
   - Replaced by format-character-chat-response-with-history

8. **format-trivia-response** (src/utils/response-formatting.lisp)
   - Legacy single-message trivia formatter
   - Replaced by format-trivia-response-with-history

#### HTML Template Generation Functions (16)
9. **escape-html-char** (src/utils/html-common.lisp)
   - Character-level HTML escaping helper
   - Only used internally

10. **escape-html-attribute** (src/utils/html-common.lisp)
    - HTML attribute escaping
    - No call sites found

11. **sanitize-html-id** (src/utils/html-common.lisp)
    - HTML ID sanitization
    - No call sites found

12. **generate-card** (src/utils/html-common.lisp)
    - DaisyUI card component generator
    - No call sites found

13. **generate-alert** (src/utils/html-common.lisp)
    - DaisyUI alert component generator
    - No call sites found

14. **generate-loading-indicator** (src/utils/html-common.lisp)
    - Loading indicator generator
    - No call sites found

15. **generate-container** (src/utils/html-common.lisp)
    - Generic container generator
    - No call sites found

16. **generate-error-display** (src/utils/html-common.lisp)
    - Error display container generator
    - No call sites found

17. **generate-result-container** (src/utils/html-common.lisp)
    - Result container generator
    - No call sites found

18. **generate-form-input** (src/utils/html-forms.lisp)
    - Form input generator
    - No call sites found

19. **generate-form-textarea** (src/utils/html-forms.lisp)
    - Form textarea generator
    - No call sites found

20. **generate-form-select** (src/utils/html-forms.lisp)
    - Form select generator
    - No call sites found

21. **generate-form-radio-group** (src/utils/html-forms.lisp)
    - Form radio group generator
    - No call sites found

22. **generate-button** (src/utils/html-forms.lisp)
    - Button generator
    - No call sites found

23. **generate-form-wrapper** (src/utils/html-forms.lisp)
    - Form wrapper generator
    - No call sites found

24. **escape-html** (src/utils/html-common.lisp)
    - NOTE: There are TWO escape-html functions:
      - One in html-common.lisp (uses escape-html-char helper)
      - One in response-formatting.lisp (uses cl-ppcre)
    - The one in response-formatting.lisp is actively used
    - The one in html-common.lisp is UNUSED
    - Recommendation: Keep response-formatting.lisp version, remove html-common.lisp version

## Recommendations

### Phase 1: Remove Unused Validation Functions (5 functions)
- Remove `validate-messages-json`
- Remove `validate-required-fields`
- Remove `validate-story-style`
- Remove `validate-character`
- Remove `sanitize-input`
- Remove exports from package.lisp
- Verify compilation and tests

### Phase 2: Remove Unused Error Handling Functions (1 function)
- Remove `handle-api-error`
- Remove exports from package.lisp
- Verify compilation and tests

### Phase 3: Remove Legacy Response Formatters (2 functions)
- Remove `format-character-chat-response`
- Remove `format-trivia-response`
- Remove exports from package.lisp
- Verify compilation and tests

### Phase 4: Remove Unused HTML Template Functions (16 functions)
- Remove all unused functions from html-common.lisp:
  - `escape-html-char`
  - `escape-html` (keep the one in response-formatting.lisp)
  - `escape-html-attribute`
  - `sanitize-html-id`
  - `generate-card`
  - `generate-alert`
  - `generate-loading-indicator`
  - `generate-container`
  - `generate-error-display`
  - `generate-result-container`
- Remove all unused functions from html-forms.lisp:
  - `generate-form-input`
  - `generate-form-textarea`
  - `generate-form-select`
  - `generate-form-radio-group`
  - `generate-button`
  - `generate-form-wrapper`
- Remove exports from package.lisp
- Verify compilation and tests

### Phase 5: Consider Removing Entire Files
After removing all unused functions, consider if these files should be removed entirely:
- **html-common.lisp**: If all functions are removed, delete the file
- **html-forms.lisp**: If all functions are removed, delete the file
- Update kabotan.asd to remove file references
- Verify compilation and tests

## Notes

- All identified functions are truly unused (zero call sites)
- Most functions are legacy code from REST API architecture or initial HTMX migration
- HTML template generation functions were likely created but never integrated into handlers
- Removal will not break any functionality
- Each removal should be followed by compilation and test verification
- Total functions to remove: 24
- Total files that may be deleted: 2 (html-common.lisp, html-forms.lisp)

## Impact Assessment

### Low Risk Removals
- All validation functions (5): No call sites found
- Error handling function (1): No call sites found
- Legacy response formatters (2): Replaced by newer versions
- HTML template functions (16): Never integrated into handlers

### Files to Update
- **src/package.lisp**: Remove 24 exports
- **src/utils/validation.lisp**: Remove 5 functions
- **src/utils/error-handling.lisp**: Remove 1 function
- **src/utils/response-formatting.lisp**: Remove 2 functions
- **src/utils/html-common.lisp**: Remove all 10 functions (or delete file)
- **src/utils/html-forms.lisp**: Remove all 6 functions (or delete file)
- **kabotan.asd**: Remove file references if files are deleted

### Test Files to Review
- **tests/validation-tests.lisp**: May contain tests for removed validation functions
- **tests/error-handling-tests.lisp**: May contain tests for removed error handling functions
- **tests/response-formatting-tests.lisp**: May contain tests for removed formatters
- **tests/html-common-tests.lisp**: May contain tests for removed HTML functions
- **tests/html-forms-tests.lisp**: May contain tests for removed HTML functions
