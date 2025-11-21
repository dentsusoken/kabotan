# Dead Code Analysis Report

## Analysis Date
2024-11-18

## Summary
This document lists all functions that are exported in `package.lisp` but are never called anywhere in the codebase. These functions are candidates for removal as they represent dead code.

## Methodology
1. Extracted all exported functions from `src/package.lisp`
2. Searched for each function's usage across the entire codebase (excluding `package.lisp`)
3. Verified that functions with zero call sites are not part of a public API
4. Categorized findings by functional area

## Confirmed Dead Code

### Validation Functions (src/utils/validation.lisp)

#### 1. `validate-language`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: This function validates language codes ('ja' or 'en'). However, the codebase uses inline validation or other methods instead.
- **Recommendation**: Remove from exports and codebase

#### 2. `validate-non-empty-string`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Validates that a string is not empty or nil. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 3. `validate-story-style`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Validates story style parameters ('gothic', 'parody', 'classic'). Not used in current HTMX architecture.
- **Recommendation**: Remove from exports and codebase

#### 4. `validate-character`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Validates character selection ('dracula', 'witch', 'jack-o-lantern'). Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 5. `sanitize-input`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Sanitizes user input to prevent injection attacks. The codebase uses `escape-html` instead.
- **Recommendation**: Remove from exports and codebase

### Response Formatting Functions (src/utils/response-formatting.lisp)

#### 6. `format-monster-diagnostic-response`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy function for formatting monster diagnostic responses. Replaced by inline HTML generation in handlers.
- **Recommendation**: Remove from exports and codebase

#### 7. `format-story-response`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy function for formatting story responses. Replaced by inline HTML generation in handlers.
- **Recommendation**: Remove from exports and codebase

#### 8. `format-spell-generator-response`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy function for formatting spell generator responses. Replaced by inline HTML generation in handlers.
- **Recommendation**: Remove from exports and codebase

### HTML Template Generation Functions (src/utils/html-common.lisp, html-forms.lisp)

#### 9. `escape-html-char`
- **Status**: USED (internally)
- **Exported**: Yes
- **Call sites**: 2 (used by `escape-html` and `escape-html-attribute` in html-common.lisp)
- **Notes**: Helper function used internally by `escape-html` and `escape-html-attribute`. Should not be exported as it's an internal implementation detail.
- **Recommendation**: Remove from exports (keep function for internal use)

#### 10. `escape-html-attribute`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Function for escaping HTML attributes. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 11. `sanitize-html-id`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Function for sanitizing HTML IDs. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 12. `generate-card`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic card generation function. Not used in current HTMX architecture.
- **Recommendation**: Remove from exports and codebase

#### 13. `generate-alert`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic alert generation function. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 14. `generate-form-input`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic form input generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 15. `generate-form-textarea`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic textarea generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 16. `generate-form-select`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic select generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 17. `generate-form-radio-group`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic radio group generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 18. `generate-button`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic button generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 19. `generate-loading-indicator`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic loading indicator generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 20. `generate-form-wrapper`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic form wrapper generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 21. `generate-container`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic container generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 22. `generate-error-display`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic error display generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 23. `generate-result-container`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Generic result container generation. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

### Prompt Builder Functions (src/services/prompt-builder.lisp)

#### 24. `build-monster-diagnostic-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy prompt builder. Current implementation uses system prompts with messages API.
- **Recommendation**: Remove from exports and codebase

#### 25. `build-story-generator-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy prompt builder. Current implementation uses system prompts with messages API.
- **Recommendation**: Remove from exports and codebase

#### 26. `build-daily-spell-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Legacy prompt builder. Current implementation uses system prompts with messages API.
- **Recommendation**: Remove from exports and codebase

#### 27. `build-character-chat-system-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: System prompt builder. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 28. `build-trivia-bot-system-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: System prompt builder. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

#### 29. `build-story-generator-system-prompt`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: System prompt builder. Not used in current implementation.
- **Recommendation**: Remove from exports and codebase

### API Handler Functions (src/api/handlers/*.lisp)

#### 30. `handle-daily-spell-request`
- **Status**: UNUSED
- **Exported**: Yes
- **Call sites**: 0
- **Notes**: Handler function that is exported but never registered in API routes. The actual endpoint uses `handle-spell-generator-request` instead.
- **Recommendation**: Remove from exports and codebase

## Functions to Keep (Used in Codebase)

The following exported functions ARE used and should be kept:

### Core Functions
- `main` - Application entry point
- `setup-halloween-api` - API setup
- `get-param` - Parameter extraction utility

### Validation Functions (Used)
- (None - all validation functions are unused)

### Error Handling
- `handle-api-error` - Used in error handling
- `format-error-response` - Used in error responses
- `log-error` - Used for error logging
- `*suppress-error-logs*` - Configuration variable

### Logging
- `log-llm-request` - Used for LLM logging
- `log-llm-response` - Used for LLM logging
- `log-llm-error` - Used for LLM logging
- `log-llm-retry` - Used for LLM logging

### Response Formatting (Used)
- `format-html-response` - Used in handlers
- `escape-html` - Used extensively for HTML escaping

### HTML Generation (Used)
- (None - all generic HTML generation functions are unused)

### Session Management (All Used)
- All session-related functions are actively used

### LLM Service (All Used)
- All LLM client functions are actively used

### Streaming (All Used)
- All streaming-related functions are actively used

### Handler Utilities (All Used)
- All handler utility functions are actively used

### Language Handler (All Used)
- All language handler functions are actively used

### API Handlers (Used)
- `handle-monster-diagnostic-request`
- `handle-monster-diagnostic-streaming`
- `handle-story-generator-request`
- `handle-story-generator-streaming`
- `handle-character-chat-request`
- `handle-character-chat-streaming`
- `handle-trivia-bot-request`
- `handle-trivia-bot-streaming`
- `handle-spell-generator-request`
- `handle-spell-generator-streaming`

## Summary Statistics

- **Total Exported Functions**: ~150
- **Confirmed Dead Code Functions**: 30 (to be removed completely)
- **Functions to Unexport**: 1 (to keep internally but remove from exports)
- **Functions to Keep**: ~119
- **Percentage of Dead Code**: ~21%

## Removal Plan

The dead code should be removed in the following order:

1. **Phase 1**: Remove unused validation functions (5 functions)
2. **Phase 2**: Remove unused response formatting functions (3 functions)
3. **Phase 3**: Remove unused HTML template generation functions (16 functions)
4. **Phase 4**: Remove unused prompt builder functions (6 functions)
5. **Phase 5**: Remove unused API handler functions (1 function)

After each phase:
- Remove function definitions from source files
- Remove exports from `package.lisp`
- Verify compilation succeeds
- Verify test suite passes

## Notes

- All identified dead code is from the legacy REST API architecture
- The HTMX migration has made these functions obsolete
- No public API contracts are broken by removing these functions
- All functions are internal to the Kabotan application
