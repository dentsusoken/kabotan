# Design Document

## Overview

This design document outlines the refactoring approach for the Kabotan codebase to improve code organization, reduce duplication, and ensure compliance with coding standards. The refactoring will maintain all existing functionality while restructuring code for better maintainability.

### Current Issues

1. **Code Duplication**: Streaming and non-streaming handlers contain nearly identical validation, parameter extraction, and error handling logic
2. **Excessive Nesting**: Some handler functions exceed the 6-level nesting depth limit
3. **Mixed Responsibilities**: Handler functions combine parameter extraction, validation, business logic, and response formatting
4. **Inconsistent Patterns**: Error handling and response formatting patterns vary across handlers

### Refactoring Goals

1. Extract common logic into reusable utility functions
2. Reduce nesting depth to 6 levels or less
3. Separate concerns: parameter extraction, validation, business logic, response formatting
4. Establish consistent patterns across all handlers
5. Maintain 100% backward compatibility

## Architecture

### Current Structure

```
src/
├── api/
│   ├── halloween-api.lisp          # Route definitions
│   └── handlers/
│       ├── monster-diagnostic-handler.lisp
│       ├── story-generator-handler.lisp
│       ├── character-chat-handler.lisp
│       ├── trivia-bot-handler.lisp
│       └── spell-generator-handler.lisp
├── services/
│   ├── llm-service.lisp            # LLM API calls
│   └── prompt-builder.lisp         # Prompt construction
└── utils/
    ├── validation.lisp             # Input validation
    ├── error-handling.lisp         # Error handling
    ├── response-formatting.lisp    # Response formatting
    └── streaming.lisp              # Streaming utilities
```

### Refactored Structure

The directory structure remains the same, but internal organization improves:

```
src/
├── api/
│   ├── halloween-api.lisp          # Route definitions (unchanged)
│   └── handlers/
│       ├── monster-diagnostic-handler.lisp  # Refactored
│       ├── story-generator-handler.lisp     # Refactored
│       ├── character-chat-handler.lisp      # Refactored
│       ├── trivia-bot-handler.lisp          # Refactored
│       └── spell-generator-handler.lisp     # Refactored
├── services/
│   ├── llm-service.lisp            # Unchanged
│   └── prompt-builder.lisp         # Unchanged
└── utils/
    ├── validation.lisp             # Enhanced with new validators
    ├── error-handling.lisp         # Enhanced with new error handlers
    ├── response-formatting.lisp    # Unchanged
    ├── streaming.lisp              # Enhanced with new utilities
    └── handler-utils.lisp          # NEW: Common handler utilities
```

## Components and Interfaces

### 1. Handler Utilities Module (NEW)

**File**: `src/utils/handler-utils.lisp`

This new module provides common utilities for all handlers to reduce duplication.

#### Functions

```lisp
;; Parameter extraction with validation
(defun extract-and-validate-language (params)
  "Extract and validate language parameter, defaulting to 'en'"
  -> (values language valid-p))

;; Common validation patterns
(defun validate-required-params (params required-keys)
  "Validate that all required parameters are present and non-empty"
  -> (values valid-p missing-keys))

;; Error response builders
(defun build-validation-error-response (message language &key streaming-p)
  "Build appropriate error response for validation errors"
  -> response)

(defun build-service-error-response (message language &key streaming-p)
  "Build appropriate error response for service errors"
  -> response)

;; Handler wrapper for consistent error handling
(defun wrap-handler (handler-fn &key streaming-p)
  "Wrap handler function with consistent error handling"
  -> wrapped-handler-fn)
```

### 2. Enhanced Validation Module

**File**: `src/utils/validation.lisp`

Add new validation functions to support refactored handlers.

#### New Functions

```lisp
(defun validate-messages-json (messages-json)
  "Validate and parse messages JSON parameter"
  -> (values messages-list error-message))

(defun validate-required-fields (params field-specs)
  "Validate multiple required fields at once"
  -> (values valid-p error-details))
```

### 3. Enhanced Streaming Utilities

**File**: `src/utils/streaming.lisp`

Add utilities to simplify streaming handler implementation.

#### New Functions

```lisp
(defun create-streaming-handler (prompt-fn callback-fn &key temperature)
  "Create a standardized streaming handler"
  -> handler-fn)

(defun handle-streaming-error (condition language)
  "Handle errors in streaming context"
  -> error-response)
```

### 4. Refactored Handler Pattern

Each handler will follow this consistent structure:

```lisp
;; Non-streaming handler
(defun handle-<feature>-request (params)
  "Handler documentation"
  (wrap-handler
   (lambda ()
     (multiple-value-bind (extracted-params error)
         (extract-<feature>-params params)
       (if error
           (build-validation-error-response error language)
           (let ((result (process-<feature>-request extracted-params)))
             (format-<feature>-response result language)))))
   :streaming-p nil))

;; Streaming handler
(defun handle-<feature>-streaming (params)
  "Handler documentation"
  (wrap-handler
   (lambda ()
     (multiple-value-bind (extracted-params error)
         (extract-<feature>-params params)
       (if error
           (build-validation-error-response error language :streaming-p t)
           (create-streaming-handler
            (lambda () (build-<feature>-prompt extracted-params))
            (lambda (chunk writer) (funcall writer (format-sse-data chunk)))
            :temperature <temp>))))
   :streaming-p t))

;; Shared parameter extraction
(defun extract-<feature>-params (params)
  "Extract and validate parameters for <feature>"
  -> (values param-plist error-message))

;; Shared business logic
(defun process-<feature>-request (params)
  "Process <feature> request with validated parameters"
  -> result)
```

## Data Models

### Parameter Extraction Result

```lisp
;; Success case
(:language "en"
 :param1 "value1"
 :param2 "value2"
 ...)

;; Error case
nil  ; with error message as second return value
```

### Validation Result

```lisp
;; Multiple values return pattern
(values result-or-nil error-message-or-nil)
```

## Error Handling

### Error Handling Strategy

1. **Validation Errors**: Return 400 with descriptive error message
2. **Service Errors**: Return 500 with generic error message
3. **Unexpected Errors**: Log and return 500 with generic error message

### Error Response Formats

#### Non-Streaming

```lisp
`(status-code
  (:content-type "text/html; charset=utf-8")
  (formatted-error-html))
```

#### Streaming

```lisp
`(status-code
  (:content-type "application/json")
  (json-error-string))
```

### Consistent Error Handling Pattern

All handlers will use the `wrap-handler` function which provides:

1. Top-level error catching
2. Consistent error logging
3. Appropriate error response formatting based on streaming mode
4. Language-aware error messages

## Testing Strategy

### Testing Approach

1. **Preserve Existing Tests**: All existing E2E tests must continue to pass
2. **No New Unit Tests Required**: Focus on maintaining existing functionality
3. **Manual Verification**: Test each endpoint manually after refactoring
4. **Incremental Refactoring**: Refactor one handler at a time and verify

### Test Verification Steps

For each refactored handler:

1. Run existing E2E tests for that feature
2. Verify both streaming and non-streaming endpoints
3. Test error cases (missing parameters, invalid inputs)
4. Verify language switching (en/ja)
5. Check error logging behavior

### Test Execution

```bash
# Run all E2E tests
npx playwright test --reporter=line

# Run specific feature tests
npx playwright test e2e-tests/monster-diagnostic.spec.js --reporter=line
npx playwright test e2e-tests/story-generator.spec.js --reporter=line
npx playwright test e2e-tests/character-chat.spec.js --reporter=line
npx playwright test e2e-tests/trivia-bot.spec.js --reporter=line
npx playwright test e2e-tests/spell-generator.spec.js --reporter=line
```

## Implementation Strategy

### Phase 1: Create Utility Functions

1. Create `handler-utils.lisp` with common utilities
2. Add new validation functions to `validation.lisp`
3. Add new streaming utilities to `streaming.lisp`
4. Update `package.lisp` to export new functions

### Phase 2: Refactor Handlers (One at a Time)

For each handler:

1. Extract parameter extraction logic into separate function
2. Extract business logic into separate function
3. Simplify handler using new utilities
4. Reduce nesting depth to ≤6 levels
5. Run E2E tests to verify functionality

Handler refactoring order:

1. `spell-generator-handler.lisp` (simplest, good starting point)
2. `monster-diagnostic-handler.lisp`
3. `story-generator-handler.lisp`
4. `character-chat-handler.lisp`
5. `trivia-bot-handler.lisp`

### Phase 3: Verification

1. Run full E2E test suite
2. Manual testing of all endpoints
3. Code review for nesting depth compliance
4. Code review for duplication elimination

## Nesting Depth Reduction

### Current Nesting Issues

Example from `monster-diagnostic-handler.lisp`:

```lisp
(defun handle-monster-diagnostic-streaming (params)
  (handler-case                                    ; Level 1
      (let* (...)                                  ; Level 2
        (unless ...)                               ; Level 3
        (if (not ...)                              ; Level 3
            ...
            (let (...)                             ; Level 4
              (let (...)                           ; Level 5
                (create-sse-response               ; Level 6
                 (lambda (writer)                  ; Level 7 - EXCEEDS LIMIT
                   (let ((result ...))             ; Level 8
                     (if (eq result :error)        ; Level 9
                         ...
                         ...))))))))
    (error ...)))
```

### Refactored Approach

```lisp
(defun handle-monster-diagnostic-streaming (params)
  (wrap-handler                                    ; Level 1
   (lambda ()                                      ; Level 2
     (multiple-value-bind (params error)           ; Level 3
         (extract-monster-diagnostic-params params)
       (if error                                   ; Level 4
           (build-validation-error-response ...)
           (create-streaming-handler               ; Level 4
            (lambda () (build-prompt params))      ; Level 5
            #'send-sse-chunk                       ; Level 5
            :temperature 0.85))))
   :streaming-p t))

;; Helper function - separate nesting context
(defun extract-monster-diagnostic-params (params)
  (let ((language (extract-and-validate-language params)))  ; Level 1
    (if (validate-required-params ...)                      ; Level 2
        (values (list :language language ...) nil)
        (values nil "Validation error"))))
```

By extracting logic into separate functions, each function maintains shallow nesting while preserving all functionality.

## Backward Compatibility

### Guarantees

1. **API Endpoints**: All endpoints remain unchanged
2. **Request Parameters**: All parameter names and formats remain unchanged
3. **Response Formats**: All response formats remain unchanged
4. **Error Behavior**: Error responses maintain same structure
5. **Streaming Protocol**: SSE format remains unchanged

### Verification

- All existing E2E tests must pass without modification
- No changes to frontend code required
- No changes to API contracts
