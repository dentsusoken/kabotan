# Implementation Plan

- [x] 1. Establish baseline and prepare for refactoring
  - Run complete test suite and record results
  - Document current test count and coverage
  - Create backup branch for safety
  - _Requirements: 8.1, 8.2, 10.1_

- [x] 2. Split test suite (tests/tests.lisp)
- [x] 2.1 Create validation-tests.lisp
  - Extract validation suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.2 Create error-handling-tests.lisp
  - Extract error handling suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.3 Create response-formatting-tests.lisp
  - Extract response formatting suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.4 Create language-handler-tests.lisp
  - Extract language handler suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.5 Create prompt-builder-tests.lisp
  - Extract prompt builder suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.6 Create llm-service-tests.lisp
  - Extract LLM service suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.7 Create streaming-tests.lisp
  - Extract streaming suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.8 Create api-endpoint-tests.lisp
  - Extract API endpoint suite tests from tests.lisp
  - Update test package to include new file
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2.9 Update kabotan-test.asd with new test files
  - Add all new test files to system definition
  - Ensure correct load order
  - _Requirements: 7.1, 7.2, 7.5_

- [x] 2.10 Remove original tests.lisp or reduce to test runner only
  - Keep only the main test suite definition and runner
  - Remove all extracted test code
  - _Requirements: 1.3_

- [x] 2.11 Verify all tests pass after split
  - Run complete test suite
  - Verify test count matches baseline
  - _Requirements: 1.4, 1.5, 8.1, 8.2_

- [x] 3. Split LLM service module (src/services/llm-service.lisp)
- [x] 3.1 Create llm-client.lisp
  - Extract core API functions (call-openai-api, call-openai-api-with-messages)
  - Extract configuration variables (*model*, *model-host*)
  - Extract test override variables
  - _Requirements: 2.1, 2.2_

- [x] 3.2 Create llm-retry.lisp
  - Extract retry wrapper functions (call-llm, call-llm-with-retry)
  - Extract message functions (call-llm-with-messages, call-llm-with-messages-retry)
  - Extract validation functions (validate-message-role, messages-to-json-array)
  - _Requirements: 2.1, 2.2_

- [x] 3.3 Create llm-streaming.lisp
  - Extract streaming API functions
  - Extract streaming configuration variables
  - _Requirements: 2.1, 2.2_

- [x] 3.4 Update kabotan.asd with new LLM service files
  - Add llm-client.lisp, llm-retry.lisp, llm-streaming.lisp
  - Ensure correct load order (client → retry, client → streaming)
  - _Requirements: 2.5, 7.1, 7.2_

- [x] 3.5 Remove original llm-service.lisp
  - Delete the original file after verifying all code is migrated
  - _Requirements: 2.1_

- [x] 3.6 Verify LLM service tests pass
  - Run LLM service test suite
  - Verify all functionality preserved
  - _Requirements: 2.3, 2.4, 8.3_

- [-] 4. Split HTML templates module (src/utils/html-templates.lisp)
- [x] 4.1 Create html-common.lisp
  - Extract HTML escaping functions
  - Extract basic DaisyUI components (card, alert, loading, container, error display)
  - _Requirements: 3.1, 3.2_

- [x] 4.2 Create html-forms.lisp
  - Extract form input components
  - Extract form wrapper and button components
  - _Requirements: 3.1, 3.2_

- [x] 4.3 Create html-features.lisp
  - Extract feature-specific form generators (spell, monster, story)
  - _Requirements: 3.1, 3.2_

- [x] 4.4 Create html-chat.lisp
  - Extract chat and conversation UI components
  - Extract character chat and trivia bot forms
  - _Requirements: 3.1, 3.2_

- [x] 4.5 Update kabotan.asd with new HTML template files
  - Add html-common.lisp, html-forms.lisp, html-features.lisp, html-chat.lisp
  - Ensure correct load order (common → forms → features/chat)
  - _Requirements: 7.1, 7.2_

- [x] 4.6 Remove original html-templates.lisp
  - Delete the original file after verifying all code is migrated
  - _Requirements: 3.3_

- [x] 4.7 Verify HTML template tests pass
  - Run HTML template test suite
  - Verify all templates produce identical output
  - _Requirements: 3.4, 3.5, 8.3_

- [x] 5. Split streaming module (src/utils/streaming.lisp)
- [x] 5.1 Create sse-protocol.lisp
  - Extract SSE protocol utilities (parse, format functions)
  - Extract create-sse-response function
  - _Requirements: 4.1, 4.2_

- [x] 5.2 Create streaming-handlers.lisp
  - Extract HTMX streaming handler functions
  - Extract error handling functions
  - _Requirements: 4.1, 4.2_

- [x] 5.3 Update kabotan.asd with new streaming files
  - Add sse-protocol.lisp, streaming-handlers.lisp
  - Ensure correct load order (protocol → handlers)
  - _Requirements: 7.1, 7.2_

- [x] 5.4 Remove original streaming.lisp
  - Delete the original file after verifying all code is migrated
  - _Requirements: 4.1_

- [x] 5.5 Verify streaming tests pass
  - Run streaming test suite
  - Verify all streaming functionality preserved
  - _Requirements: 4.4, 4.5, 8.3_

- [x] 6. Split HTML template tests (tests/html-templates-tests.lisp)
- [x] 6.1 Create html-common-tests.lisp
  - Extract tests for escaping and common components
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 6.2 Create html-forms-tests.lisp
  - Extract tests for form components
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 6.3 Create html-features-tests.lisp
  - Extract tests for feature forms and chat components
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 6.4 Update kabotan-test.asd with new HTML test files
  - Add all new HTML test files to system definition
  - Ensure correct load order
  - _Requirements: 7.1, 7.2, 7.5_

- [x] 6.5 Remove original html-templates-tests.lisp
  - Delete the original file after verifying all tests are migrated
  - _Requirements: 1.3_

- [x] 6.6 Verify all HTML tests pass after split
  - Run complete HTML test suite
  - Verify test count matches baseline
  - _Requirements: 1.4, 1.5, 8.1, 8.2_

- [-] 7. Review and refactor functions exceeding limits
- [x] 7.1 Identify functions exceeding 100 lines
  - Scan all source files for long functions
  - Create list of functions to refactor
  - _Requirements: 6.1_

- [x] 7.2 Identify functions with excessive nesting depth
  - Scan all source files for deep nesting (> 6 levels)
  - Create list of functions to refactor
  - _Requirements: 5.1_

- [x] 7.3 Refactor identified functions
  - Extract helper functions to reduce length and nesting
  - Maintain original function interfaces
  - Run tests after each refactoring
  - _Requirements: 5.1, 5.3, 6.2, 6.3_

- [x] 7.4 Verify all tests pass after function refactoring
  - Run complete test suite
  - Verify no regressions
  - _Requirements: 5.4, 6.4_

- [x] 8. Implement property-based tests for behavior preservation
- [x] 8.1 Set up cl-quickcheck library
  - Add cl-quickcheck to test system dependencies
  - Create property test utilities
  - _Requirements: Testing Strategy_

- [x] 8.2 Write property test for behavior preservation
  - **Property 1: Refactoring preserves behavior**
  - Generate random inputs for key functions
  - Verify outputs match expected behavior
  - Run 100+ iterations
  - _Requirements: 2.4, 3.5, 4.5, 5.3, 6.3, 8.5_
  - **Validates: Requirements 2.4, 3.4, 3.5, 4.4, 4.5, 5.3, 6.3, 8.5**

- [x] 8.3 Write property test for test coverage maintenance
  - **Property 2: Test coverage is maintained**
  - Count test cases before and after refactoring
  - Verify count is maintained or increased
  - _Requirements: 1.4, 8.4_
  - **Validates: Requirements 1.4, 8.4**

- [x] 8.4 Write property test for export preservation
  - **Property 3: Exported functions are preserved**
  - List all exported functions before refactoring
  - Verify all are still exported after refactoring
  - _Requirements: 2.3_
  - **Validates: Requirements 2.3**

- [x] 9. Final verification and documentation
- [x] 9.1 Verify all files comply with coding standards
  - Check all files are under 500 lines
  - Check all functions are under 100 lines
  - Check no nesting depth exceeds 6 levels
  - _Requirements: 1.3, 5.5, 6.5, 10.5_

- [x] 9.2 Run complete test suite
  - Execute all unit tests
  - Execute all integration tests
  - Verify no failures or errors
  - _Requirements: 8.1, 8.2, 8.3_

- [x] 9.3 Run application and verify functionality
  - Start application
  - Test each feature manually
  - Verify no regressions
  - _Requirements: 10.3, 10.4_

- [x] 9.4 Update structure.md documentation
  - Document new file organization
  - Explain module dependencies
  - Add guidelines for new functionality
  - _Requirements: 9.1, 9.2, 9.3, 9.4_

- [x] 9.5 Update ASDF system definitions with comments
  - Add comments explaining module purposes
  - Document load order rationale
  - _Requirements: 7.1, 7.2_

- [x] 10. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.
