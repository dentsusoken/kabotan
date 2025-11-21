# Requirements Document

## Introduction

This specification addresses the reorganization of E2E tests to clearly separate UI tests, integration tests, and remove obsolete test infrastructure. The current test suite has several issues: integration tests are using mocks (defeating their purpose), obsolete streaming test targets exist, and the test organization doesn't clearly reflect the testing goals.

## Glossary

- **E2E Test**: End-to-end test using Playwright to verify application behavior from a user perspective
- **UI Test**: Test that verifies user interface behavior without requiring real LLM API calls (uses mocks)
- **Integration Test**: Test that verifies the application works correctly with real LLM API calls (no mocks)
- **Mock**: Simulated API response used to test UI behavior without making real API calls
- **Streaming**: Server-Sent Events (SSE) based response delivery where content is sent incrementally
- **Make Target**: Command defined in Makefile for running specific test suites
- **Playwright**: E2E testing framework used for browser automation

## Requirements

### Requirement 1

**User Story:** As a developer, I want integration tests to use real LLM API calls without mocks, so that I can verify the application works correctly with actual backend services.

#### Acceptance Criteria

1. WHEN an integration test runs THEN the system SHALL NOT use mock API responses
2. WHEN an integration test makes an LLM API call THEN the system SHALL use the real OpenAI-compatible endpoint
3. WHEN reviewing integration test files THEN the system SHALL NOT contain mock setup code in integration test suites
4. WHEN an integration test completes THEN the system SHALL have verified real end-to-end functionality

### Requirement 2

**User Story:** As a developer, I want to remove obsolete test infrastructure, so that the test suite is maintainable and doesn't contain dead code.

#### Acceptance Criteria

1. WHEN the test-e2e-streaming make target is invoked THEN the system SHALL execute valid tests or not exist
2. WHEN empty test files exist THEN the system SHALL remove them from the repository
3. WHEN obsolete test files are removed THEN the system SHALL update all references in configuration files
4. WHEN the Makefile is reviewed THEN the system SHALL contain only valid, functional test targets

### Requirement 3

**User Story:** As a developer, I want integration tests to verify core LLM features work correctly, so that I can ensure the application's main functionality is operational.

#### Acceptance Criteria

1. WHEN integration tests run THEN the system SHALL test monster diagnostic feature with real LLM
2. WHEN integration tests run THEN the system SHALL test story generator feature with real LLM
3. WHEN integration tests run THEN the system SHALL test character chat feature with real LLM
4. WHEN integration tests run THEN the system SHALL test spell generator feature with real LLM
5. WHEN integration tests run THEN the system SHALL test trivia bot feature with real LLM

### Requirement 4

**User Story:** As a developer, I want integration tests to verify multilingual support, so that I can ensure the application works correctly in both English and Japanese.

#### Acceptance Criteria

1. WHEN integration tests run THEN the system SHALL test features in English language
2. WHEN integration tests run THEN the system SHALL test features in Japanese language
3. WHEN language is switched THEN the system SHALL send the correct language parameter to the API
4. WHEN responses are received THEN the system SHALL display content in the requested language

### Requirement 5

**User Story:** As a developer, I want integration tests to verify streaming functionality, so that I can ensure Server-Sent Events work correctly with real LLM responses.

#### Acceptance Criteria

1. WHEN a streaming integration test runs THEN the system SHALL verify content is delivered incrementally
2. WHEN streaming content is received THEN the system SHALL verify the UI updates during streaming
3. WHEN streaming completes THEN the system SHALL verify the final content is complete and correct
4. WHEN streaming is tested THEN the system SHALL use real LLM API streaming endpoints

### Requirement 6

**User Story:** As a developer, I want clear separation between UI tests and integration tests, so that I can run fast UI tests during development and comprehensive integration tests before deployment.

#### Acceptance Criteria

1. WHEN UI tests run THEN the system SHALL complete in under 60 seconds total
2. WHEN integration tests run THEN the system SHALL be allowed up to 180 seconds per test
3. WHEN test files are organized THEN the system SHALL use separate directories for UI tests and integration tests
4. WHEN make targets are defined THEN the system SHALL provide separate commands for UI tests and integration tests
5. WHEN the test directory structure is reviewed THEN the system SHALL have a clear directory hierarchy separating UI tests from integration tests
