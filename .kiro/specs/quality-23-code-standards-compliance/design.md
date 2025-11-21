# Design Document

## Overview

This design document outlines the approach for refactoring the Kabotan codebase to comply with coding standards that limit file length to 500 lines, function length to 100 lines, and nesting depth to 6 levels. The refactoring will be performed incrementally, one module at a time, with tests passing after each change to ensure no functionality is broken.

The primary violations are:
- `tests/tests.lisp` (1273 lines) - Contains multiple test suites
- `src/services/llm-service.lisp` (1138 lines) - Contains core API, retry logic, and streaming
- `src/utils/html-templates.lisp` (1080 lines) - Contains all HTML generation functions
- `src/utils/streaming.lisp` (747 lines) - Contains SSE utilities and streaming handlers
- `tests/html-templates-tests.lisp` (623 lines) - Contains all HTML template tests

## Architecture

### Module Split Strategy

The refactoring follows a clear separation of concerns principle:

1. **LLM Service Split**: Separate core API client, retry logic, and streaming functionality
2. **HTML Templates Split**: Separate by component type (common, forms, features)
3. **Streaming Split**: Separate SSE protocol utilities from HTMX-specific handlers
4. **Test Suite Split**: Separate by functional area matching source code organization

### File Organization Principles

- Each file should have a single, well-defined responsibility
- Related functions should be grouped together
- Dependencies should flow in one direction (no circular dependencies)
- File names should clearly indicate their purpose
- Load order should be explicit in ASDF system definitions

### Backward Compatibility

During refactoring:
- All public APIs remain unchanged
- All function signatures remain identical
- All tests continue to pass
- The application remains runnable after each incremental change

## Components and Interfaces

### LLM Service Module Split

**Current**: `src/services/llm-service.lisp` (1138 lines)

**New Structure**:

1. `src/services/llm-client.lisp` (~300 lines)
   - Core API client functions
   - `call-openai-api`
   - `call-openai-api-with-messages`
   - Configuration variables (`*model*`, `*model-host*`)
   - Test override variables (`*llm-api-function*`, `*llm-api-messages-function*`)

2. `src/services/llm-retry.lisp` (~200 lines)
   - Retry logic wrapper functions
   - `call-llm`
   - `call-llm-with-retry`
   - `call-llm-with-messages`
   - `call-llm-with-messages-retry`
   - Message validation (`validate-message-role`, `messages-to-json-array`)

3. `src/services/llm-streaming.lisp` (~400 lines)
   - Streaming API functions
   - `call-openai-api-streaming`
   - `call-openai-api-with-messages-streaming`
   - `call-llm-streaming`
   - `call-llm-with-messages-streaming`
   - Streaming configuration variables

**Dependencies**: 
- `llm-client.lisp` has no internal dependencies
- `llm-retry.lisp` depends on `llm-client.lisp`
- `llm-streaming.lisp` depends on `llm-client.lisp`

### HTML Templates Module Split

**Current**: `src/utils/html-templates.lisp` (1080 lines)

**New Structure**:

1. `src/utils/html-common.lisp` (~150 lines)
   - HTML escaping and sanitization
   - `escape-html`, `escape-html-char`, `escape-html-attribute`
   - `sanitize-html-id`
   - Basic DaisyUI components
   - `generate-card`, `generate-alert`, `generate-loading-indicator`
   - `generate-container`, `generate-error-display`, `generate-result-container`

2. `src/utils/html-forms.lisp` (~200 lines)
   - Form input components
   - `generate-form-input`, `generate-form-textarea`
   - `generate-form-select`, `generate-form-radio-group`
   - `generate-button`, `generate-form-wrapper`

3. `src/utils/html-features.lisp` (~400 lines)
   - Feature-specific form generators
   - `generate-spell-generator-form`
   - `generate-monster-diagnostic-form`
   - `generate-story-generator-form`

4. `src/utils/html-chat.lisp` (~300 lines)
   - Chat and conversation UI components
   - `generate-chat-message`, `get-character-display-name`
   - `generate-character-chat-form`
   - `generate-trivia-message`, `generate-trivia-bot-form`

**Dependencies**:
- `html-common.lisp` has no internal dependencies
- `html-forms.lisp` depends on `html-common.lisp`
- `html-features.lisp` depends on `html-common.lisp` and `html-forms.lisp`
- `html-chat.lisp` depends on `html-common.lisp` and `html-forms.lisp`

### Streaming Module Split

**Current**: `src/utils/streaming.lisp` (747 lines)

**New Structure**:

1. `src/utils/sse-protocol.lisp` (~250 lines)
   - SSE protocol utilities
   - `parse-sse-chunk`
   - `format-sse-data`, `format-sse-event`
   - `format-sse-error`, `format-sse-html-error`
   - `create-sse-response`

2. `src/utils/streaming-handlers.lisp` (~400 lines)
   - HTMX streaming handlers
   - `create-streaming-handler`
   - `create-htmx-streaming-handler`
   - `create-htmx-streaming-handler-with-messages`
   - `handle-streaming-error`

**Dependencies**:
- `sse-protocol.lisp` has no internal dependencies
- `streaming-handlers.lisp` depends on `sse-protocol.lisp` and LLM service modules

### Test Suite Split

**Current**: `tests/tests.lisp` (1273 lines)

**New Structure**:

1. `tests/validation-tests.lisp` (~150 lines)
   - Validation suite tests
   - Input validation tests

2. `tests/error-handling-tests.lisp` (~100 lines)
   - Error handling suite tests

3. `tests/response-formatting-tests.lisp` (~100 lines)
   - Response formatting suite tests

4. `tests/language-handler-tests.lisp` (~150 lines)
   - Language handler suite tests

5. `tests/prompt-builder-tests.lisp` (~150 lines)
   - Prompt builder suite tests

6. `tests/llm-service-tests.lisp` (~300 lines)
   - LLM service suite tests
   - Core API tests
   - Retry logic tests

7. `tests/streaming-tests.lisp` (~200 lines)
   - Streaming suite tests
   - SSE protocol tests

8. `tests/api-endpoint-tests.lisp` (~100 lines)
   - API endpoint suite tests

**Current**: `tests/html-templates-tests.lisp` (623 lines)

**New Structure**:

1. `tests/html-common-tests.lisp` (~200 lines)
   - Tests for escaping and common components

2. `tests/html-forms-tests.lisp` (~200 lines)
   - Tests for form components

3. `tests/html-features-tests.lisp` (~200 lines)
   - Tests for feature-specific forms and chat components

**Dependencies**: Test files depend on their corresponding source modules

## Data Models

No data model changes are required. All existing data structures remain unchanged.

## Correctness Properties


*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

After analyzing the acceptance criteria, most requirements are structural (code organization) rather than functional. However, we can define properties for the critical functional requirements:

Property 1: Refactoring preserves behavior
*For any* function that exists before refactoring, calling it with the same inputs after refactoring should produce identical outputs and side effects
**Validates: Requirements 2.4, 3.4, 3.5, 4.4, 4.5, 5.3, 6.3, 8.5**

Property 2: Test coverage is maintained
*For any* test suite, the number of test cases after refactoring should be greater than or equal to the number before refactoring
**Validates: Requirements 1.4, 8.4**

Property 3: Exported functions are preserved
*For any* function exported from a module before refactoring, that function should still be exported (possibly from a different file) after refactoring
**Validates: Requirements 2.3**

## Error Handling

Error handling remains unchanged during refactoring. All existing error handling logic is preserved in the split modules.

### Error Handling Strategy

- Maintain all existing error conditions and error messages
- Preserve error propagation behavior across module boundaries
- Ensure error handling tests continue to pass

## Testing Strategy

### Unit Testing Approach

The refactoring will be validated primarily through existing unit tests:

1. **Pre-refactoring baseline**: Run all tests and record results
2. **Incremental validation**: After each module split, run relevant tests
3. **Post-refactoring verification**: Run all tests and compare with baseline

### Specific Test Areas

1. **LLM Service Tests**
   - Core API functionality
   - Retry logic behavior
   - Streaming functionality
   - Message validation

2. **HTML Template Tests**
   - HTML escaping and sanitization
   - Component generation
   - Form generation
   - Feature-specific templates

3. **Streaming Tests**
   - SSE protocol parsing
   - Event formatting
   - Streaming handlers

4. **Integration Tests**
   - End-to-end feature functionality
   - API endpoint behavior

### Property-Based Testing

While most requirements are structural, we will implement property-based tests for the critical functional properties:

1. **Behavior Preservation Test**
   - Generate random inputs for key functions
   - Compare outputs before and after refactoring
   - Verify identical behavior

2. **Test Coverage Test**
   - Count test cases before refactoring
   - Count test cases after refactoring
   - Verify count is maintained or increased

3. **Export Preservation Test**
   - List all exported functions before refactoring
   - List all exported functions after refactoring
   - Verify all original exports are still available

### Testing Framework

- **Unit Tests**: FiveAM framework (existing)
- **Property-Based Tests**: cl-quickcheck library
- **Test Execution**: Run after each incremental change

### Test Configuration

- All tests must pass before proceeding to next module
- Property-based tests should run at least 100 iterations
- Integration tests should be run after all modules are refactored

## Implementation Phases

### Phase 1: Test Suite Split (tests/tests.lisp)

Split the large test file into focused test modules:

1. Create individual test files for each suite
2. Update test system definition
3. Verify all tests still pass
4. Update documentation

**Success Criteria**: All tests pass, file sizes under 500 lines

### Phase 2: LLM Service Split (src/services/llm-service.lisp)

Split into client, retry, and streaming modules:

1. Create `llm-client.lisp` with core API functions
2. Create `llm-retry.lisp` with retry logic
3. Create `llm-streaming.lisp` with streaming functions
4. Update system definition
5. Run LLM service tests
6. Update documentation

**Success Criteria**: All LLM tests pass, file sizes under 500 lines

### Phase 3: HTML Templates Split (src/utils/html-templates.lisp)

Split into common, forms, features, and chat modules:

1. Create `html-common.lisp` with escaping and basic components
2. Create `html-forms.lisp` with form components
3. Create `html-features.lisp` with feature forms
4. Create `html-chat.lisp` with chat components
5. Update system definition
6. Run HTML template tests
7. Update documentation

**Success Criteria**: All HTML tests pass, file sizes under 500 lines

### Phase 4: Streaming Split (src/utils/streaming.lisp)

Split into protocol and handlers modules:

1. Create `sse-protocol.lisp` with SSE utilities
2. Create `streaming-handlers.lisp` with HTMX handlers
3. Update system definition
4. Run streaming tests
5. Update documentation

**Success Criteria**: All streaming tests pass, file sizes under 500 lines

### Phase 5: HTML Template Tests Split (tests/html-templates-tests.lisp)

Split into focused test modules:

1. Create `html-common-tests.lisp`
2. Create `html-forms-tests.lisp`
3. Create `html-features-tests.lisp`
4. Update test system definition
5. Verify all tests pass
6. Update documentation

**Success Criteria**: All tests pass, file sizes under 500 lines

### Phase 6: Function Refactoring

Review and refactor any functions exceeding limits:

1. Identify functions > 100 lines
2. Identify functions with nesting depth > 6
3. Extract helper functions as needed
4. Run tests after each refactoring
5. Update documentation

**Success Criteria**: No functions exceed 100 lines or 6 levels of nesting

### Phase 7: Final Verification

1. Run complete test suite
2. Run property-based tests
3. Verify all files under 500 lines
4. Verify all functions under 100 lines
5. Verify no nesting depth > 6
6. Update structure.md documentation
7. Run application and verify functionality

**Success Criteria**: All tests pass, all coding standards met

## Rollback Strategy

If any phase fails:

1. Revert changes for that phase
2. Analyze failure cause
3. Adjust split strategy if needed
4. Retry phase

## Documentation Updates

After refactoring:

1. Update `structure.md` with new file organization
2. Document module dependencies
3. Update ASDF system definitions
4. Add comments explaining module purposes
5. Update README if needed

## Performance Considerations

The refactoring should not impact performance:

- No algorithmic changes
- No additional function call overhead (inlining where appropriate)
- Same compilation optimizations apply
- File splitting does not affect runtime performance

## Security Considerations

No security implications from this refactoring:

- No changes to input validation
- No changes to HTML escaping
- No changes to error handling
- All security-critical code preserved exactly
