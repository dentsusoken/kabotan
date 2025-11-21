# Implementation Plan

- [x] 1. Analyze codebase for unused functions
  - Search for all exported functions in `package.lisp`
  - For each exported function, search for call sites in the codebase
  - Create a list of functions with zero call sites
  - Manually review candidates to confirm they're truly unused
  - Document findings in a temporary analysis file
  - _Requirements: 1.1, 1.2, 4.1, 4.2_

- [x] 2. Remove unused validation functions
  - Remove `validate-messages-json` function from `src/utils/validation.lisp`
  - Remove `validate-required-fields` function from `src/utils/validation.lisp`
  - Remove exports from `src/package.lisp`
  - Verify no remaining code references these functions
  - _Requirements: 1.3, 1.4, 5.3, 5.4_

- [x] 3. Verify compilation after validation function removal
  - Run compilation check: `ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit`
  - If compilation fails, identify and fix remaining references
  - _Requirements: 7.3_

- [x] 4. Verify tests after validation function removal
  - Run test suite: `make test`
  - If tests fail, identify and fix issues
  - _Requirements: 5.5, 7.4_

- [x] 5. Analyze response formatting functions
  - Review usage of `format-character-chat-response` in streaming handlers
  - Review usage of `format-trivia-response` in streaming handlers
  - Determine if these legacy single-message formatters are still needed
  - Document findings
  - _Requirements: 2.1, 2.2_

- [x] 6. Remove unused response formatting functions (if applicable)
  - Remove unused formatting functions from `src/utils/response-formatting.lisp`
  - Update handlers to use standard formatters if needed
  - Verify no remaining code references removed functions
  - _Requirements: 2.3, 2.4_

- [x] 7. Verify compilation after formatting function removal
  - Run compilation check: `ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit`
  - If compilation fails, identify and fix remaining references
  - _Requirements: 7.3_

- [x] 8. Verify tests after formatting function removal
  - Run test suite: `make test`
  - If tests fail, identify and fix issues
  - _Requirements: 2.5, 7.4_

- [x] 9. Analyze for obsolete API endpoints
  - Review all routes in `src/api/halloween-api.lisp`
  - Identify any endpoints that return JSON instead of HTML
  - Verify each endpoint has a clear purpose in HTMX architecture
  - Document findings
  - _Requirements: 3.1, 3.2_

- [x] 10. Remove obsolete API endpoints (if any found)
  - Remove route definitions from `src/api/halloween-api.lisp`
  - Remove handler functions
  - Update API documentation if needed
  - _Requirements: 3.3, 3.4, 3.5_

- [x] 11. Perform final dead code analysis
  - Search for functions that are defined but never called
  - Verify each candidate is not part of a public API
  - Create a list of confirmed dead code
  - _Requirements: 4.1, 4.2, 7.1_

- [x] 12. Remove identified dead code
  - Remove dead functions from source files
  - Remove exports from `src/package.lisp`
  - Verify no tests reference removed functions
  - _Requirements: 4.3, 4.4_

- [x] 13. Verify compilation after dead code removal
  - Run compilation check: `ros run -- --disable-debugger --eval '(ql:quickload :kabotan)' --quit`
  - If compilation fails, identify and fix remaining references
  - _Requirements: 4.5, 7.3_

- [x] 14. Verify tests after dead code removal
  - Run test suite: `make test`
  - If tests fail, identify and fix issues
  - _Requirements: 7.4_

- [x] 15. Identify and remove obsolete test files
  - Review test files for tests of removed functionality
  - Verify the tested functionality no longer exists
  - Remove obsolete test files
  - _Requirements: 6.1, 6.2, 6.3_

- [x] 16. Verify test suite after test removal
  - Run test suite: `make test`
  - Ensure all remaining tests pass
  - _Requirements: 6.4_

- [x] 17. Final verification
  - Run full compilation check
  - Run full test suite
  - Start application in background: `controlBashProcess(action: "start", command: "make run")`
  - Verify application starts without errors
  - Stop application: `controlBashProcess(action: "stop", processId: <id>)`
  - _Requirements: 7.3, 7.4_

- [x] 18. Run E2E tests for final verification
  - Run E2E test suite: `npm run test:e2e`
  - Verify all E2E tests pass
  - If any tests fail, investigate and fix issues

- [x] 19. Update documentation
  - Update steering files if they reference removed code
  - Update README if necessary
  - Update `.kiro/specs/README.md` to include this spec
  - _Requirements: 1.5, 3.5_

- [x] 20. Generate cleanup summary report
  - Create a summary document listing all removed code
  - Include statistics (number of functions removed, lines of code removed)
  - Document any issues encountered and how they were resolved
  - _Requirements: 7.5_
