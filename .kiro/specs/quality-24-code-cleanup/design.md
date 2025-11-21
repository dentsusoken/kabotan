# Design Document

## Overview

This document outlines the design for cleaning up obsolete code following the migration from a REST API architecture to an HTMX-driven hypermedia architecture. The cleanup will focus on identifying and removing:

1. Unused JSON-related functions
2. Obsolete validation functions designed for JSON payloads
3. Redundant or unused utility functions
4. Obsolete test files for removed functionality
5. Dead code that is no longer referenced

The cleanup will be performed systematically to ensure no breaking changes are introduced, with verification at each step through compilation and test execution.

## Architecture

### Current State Analysis

The codebase has successfully migrated to HTMX architecture where:
- All API handlers return HTML fragments instead of JSON
- Streaming responses use SSE with HTML fragments
- Client-side JavaScript is minimal (language management, page initialization, streaming handlers)
- All form submissions use HTMX attributes (`hx-post`, `hx-get`, etc.)

### Identified Obsolete Code Categories

#### 1. Unused Validation Functions

**Location:** `src/utils/validation.lisp`

- `validate-messages-json`: This function parses and validates JSON message arrays. However, after reviewing the codebase:
  - Character chat handler uses inline JSON parsing with `cl-json:decode-json-from-string`
  - No handlers call `validate-messages-json` directly
  - The function is exported in `package.lisp` but unused
  
- `validate-required-fields`: This function provides complex field validation with custom validators. However:
  - All current handlers use simpler `validate-required-params` from `handler-utils.lisp`
  - No handlers call `validate-required-fields`
  - The function is exported but unused

**Status:** Both functions appear to be unused legacy code from the REST API era.

#### 2. Potentially Redundant Functions

**Location:** `src/utils/response-formatting.lisp`

Several formatting functions exist that may have overlapping or obsolete functionality:

- `format-html-response`: Generic HTML wrapper with DaisyUI styling
- `format-character-chat-response`: Legacy streaming-only formatter (single message)
- `format-character-chat-response-with-history`: Current formatter (user + assistant messages)
- `format-trivia-response`: Legacy streaming-only formatter (single message)
- `format-trivia-response-with-history`: Current formatter (user + assistant messages)

**Analysis needed:** Determine if the legacy single-message formatters are still used in streaming handlers.

#### 3. Message API Tests

**Location:** `tests/message-api-tests.lisp`

This test file contains extensive tests for:
- `validate-message-role`: Still used in `llm-retry.lisp`
- `messages-to-json-array`: Still used in `llm-retry.lisp`
- `call-llm-with-messages`: Still used throughout the application
- `call-llm-with-messages-retry`: Still used throughout the application

**Status:** These tests are still relevant and should be kept.

#### 4. Unused Exports

**Location:** `src/package.lisp`

The package exports several functions that may no longer be used:
- `:validate-messages-json` - Appears unused
- `:validate-required-fields` - Appears unused

## Components and Interfaces

### Code Analysis Component

**Purpose:** Analyze the codebase to identify unused functions, dead code, and obsolete patterns.

**Methods:**
1. **Static Analysis:** Search for function definitions and their call sites
2. **Export Analysis:** Identify exported functions that are never called
3. **Test Coverage Analysis:** Identify tests for removed functionality

**Tools:**
- `grepSearch` for finding function definitions and usages
- `readFile` for analyzing function implementations
- Manual code review for complex dependencies

### Code Removal Component

**Purpose:** Safely remove identified obsolete code.

**Process:**
1. Remove function definition
2. Remove function from exports in `package.lisp`
3. Remove associated tests
4. Verify compilation succeeds
5. Verify test suite passes

### Verification Component

**Purpose:** Ensure no breaking changes are introduced.

**Checks:**
1. Compilation check: `ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit`
2. Test execution: `make test`
3. Manual review of removed code

## Data Models

### Code Element

Represents a piece of code to be analyzed or removed.

```lisp
(defstruct code-element
  name           ; Function or variable name
  type           ; :function, :variable, :test
  location       ; File path
  exported-p     ; Whether it's exported from package
  used-p         ; Whether it's used anywhere
  dependencies)  ; List of other code elements it depends on
```

### Removal Plan

Represents the plan for removing obsolete code.

```lisp
(defstruct removal-plan
  elements       ; List of code-element structs to remove
  order          ; Ordered list of removal steps
  verification)  ; Verification steps after each removal
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Compilation preservation
*For any* code removal operation, after removing the code, the system should still compile without errors
**Validates: Requirements 4.5, 7.3**

### Property 2: Test suite preservation
*For any* code removal operation (excluding test removals), after removing the code, all remaining tests should still pass
**Validates: Requirements 4.4, 5.5, 7.4**

### Property 3: Export consistency
*For any* function removal, if the function is removed from the codebase, it should also be removed from the package exports
**Validates: Requirements 1.4**

### Property 4: Reference elimination
*For any* function removal, after removing the function, no remaining code should reference the removed function name
**Validates: Requirements 1.4, 4.4**

## Error Handling

### Compilation Errors

If compilation fails after code removal:
1. Identify the error message
2. Search for remaining references to removed code
3. Remove or update the referencing code
4. Retry compilation

### Test Failures

If tests fail after code removal:
1. Identify which tests are failing
2. Determine if the failure is due to:
   - Missing functionality (revert removal)
   - Test referencing removed code (remove test)
   - Unrelated issue (investigate separately)
3. Take appropriate action

### Rollback Strategy

For each removal operation:
1. Use version control to track changes
2. If verification fails, revert the specific change
3. Document why the removal failed
4. Re-analyze dependencies before attempting again

## Testing Strategy

### Unit Testing

Unit tests will verify:
- Specific examples of code removal (e.g., removing a known unused function)
- Compilation succeeds after removal
- Specific tests pass after removal

### Property-Based Testing

Property-based tests will verify:
- **Property 1 (Compilation preservation):** Generate random code removal scenarios and verify compilation succeeds
- **Property 2 (Test suite preservation):** Generate random code removal scenarios and verify tests pass
- **Property 3 (Export consistency):** Generate random function removals and verify exports are updated
- **Property 4 (Reference elimination):** Generate random function removals and verify no references remain

**Property Testing Library:** Since Common Lisp doesn't have a widely-used property-based testing library like QuickCheck, we will:
1. Use FiveAM for unit tests
2. Implement manual property verification through systematic testing
3. Run compilation and test suite after each removal as a form of property verification

**Test Configuration:**
- Each property verification will be run after every code removal operation
- Compilation check: `ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit`
- Test suite check: `make test`

### Integration Testing

Integration tests will verify:
- The application starts successfully after cleanup
- All API endpoints still function correctly
- E2E tests still pass

### Manual Testing

Manual verification will include:
- Code review of removed functions to ensure they're truly unused
- Review of git diff to ensure no unintended changes
- Spot-check of key functionality through the web interface

## Implementation Approach

### Phase 1: Analysis

1. Identify all exported functions in `package.lisp`
2. For each exported function, search for call sites in the codebase
3. Mark functions with zero call sites as candidates for removal
4. Manually review candidates to confirm they're truly unused

### Phase 2: Validation Function Cleanup

1. Remove `validate-messages-json` function
2. Remove `validate-required-fields` function
3. Remove exports from `package.lisp`
4. Verify compilation and tests

### Phase 3: Response Formatting Cleanup

1. Analyze usage of legacy formatting functions
2. If unused, remove legacy single-message formatters
3. Verify compilation and tests

### Phase 4: Dead Code Analysis

1. Run final analysis to identify any remaining dead code
2. Generate report of unused functions
3. Remove identified dead code
4. Verify compilation and tests

### Phase 5: Documentation Update

1. Update steering files if they reference removed code
2. Update README if necessary
3. Generate summary report of all removed code

## Verification Checklist

After each removal operation:
- [ ] Code compiles without errors
- [ ] All tests pass
- [ ] No references to removed code remain
- [ ] Exports are updated in `package.lisp`
- [ ] Git commit created with clear message

After all cleanup:
- [ ] Application starts successfully
- [ ] E2E tests pass
- [ ] Manual spot-check of key features
- [ ] Documentation updated
- [ ] Summary report generated
