# Requirements Document

## Introduction

This document specifies requirements for cleaning up obsolete code following the migration to an HTMX-driven architecture. The Kabotan application has transitioned from a REST API architecture (returning JSON) to an HTMX-driven hypermedia architecture (returning HTML fragments). This migration has left behind unused code, redundant utilities, and obsolete patterns that should be removed to improve maintainability and reduce technical debt.

## Glossary

- **HTMX Architecture**: A hypermedia-driven architecture where the server returns HTML fragments instead of JSON, and the client uses declarative HTML attributes for interactions
- **REST API**: A traditional API architecture where the server returns JSON data and the client handles rendering
- **HTML Fragment**: A partial HTML snippet returned by the server that can be inserted into the DOM
- **SSE (Server-Sent Events)**: A protocol for streaming data from server to client
- **Legacy Code**: Code that was written for the previous REST API architecture but is no longer needed
- **Dead Code**: Functions, modules, or files that are no longer called or used anywhere in the codebase

## Requirements

### Requirement 1

**User Story:** As a developer, I want to identify and remove unused JSON-related functions, so that the codebase only contains code relevant to the current HTMX architecture

#### Acceptance Criteria

1. WHEN analyzing the codebase THEN the system SHALL identify all functions that generate JSON responses for API endpoints
2. WHEN a JSON response function is identified THEN the system SHALL verify whether it is still used in the current HTMX architecture
3. WHEN a JSON response function is confirmed unused THEN the system SHALL remove it from the codebase
4. WHEN removing JSON functions THEN the system SHALL ensure no remaining code references the removed functions
5. WHEN JSON functions are removed THEN the system SHALL update any documentation that references them

### Requirement 2

**User Story:** As a developer, I want to identify and remove redundant HTML formatting functions, so that the codebase has a single, consistent way to format responses

#### Acceptance Criteria

1. WHEN analyzing HTML formatting utilities THEN the system SHALL identify duplicate or overlapping functionality
2. WHEN duplicate HTML formatters are found THEN the system SHALL determine which version is actively used
3. WHEN an unused HTML formatter is identified THEN the system SHALL remove it from the codebase
4. WHEN removing HTML formatters THEN the system SHALL ensure all handlers use the remaining standard formatters
5. WHEN HTML formatters are consolidated THEN the system SHALL verify all tests still pass

### Requirement 3

**User Story:** As a developer, I want to identify and remove obsolete API endpoints, so that the application only exposes necessary endpoints

#### Acceptance Criteria

1. WHEN reviewing API routes THEN the system SHALL identify endpoints that return JSON instead of HTML
2. WHEN a JSON-returning endpoint is found THEN the system SHALL verify whether it has an HTMX equivalent
3. WHEN an obsolete endpoint is confirmed THEN the system SHALL remove its route definition
4. WHEN removing an endpoint THEN the system SHALL remove its handler function
5. WHEN endpoints are removed THEN the system SHALL update API documentation

### Requirement 4

**User Story:** As a developer, I want to identify and remove unused utility functions, so that the codebase is lean and maintainable

#### Acceptance Criteria

1. WHEN analyzing utility modules THEN the system SHALL identify functions that are not called anywhere
2. WHEN an unused utility function is found THEN the system SHALL verify it is not part of a public API
3. WHEN a utility function is confirmed unused THEN the system SHALL remove it
4. WHEN removing utility functions THEN the system SHALL ensure no tests reference them
5. WHEN utility functions are removed THEN the system SHALL verify the application still builds successfully

### Requirement 5

**User Story:** As a developer, I want to identify and remove obsolete validation functions, so that validation logic is consistent and not duplicated

#### Acceptance Criteria

1. WHEN analyzing validation utilities THEN the system SHALL identify validation functions specific to JSON payloads
2. WHEN a JSON validation function is found THEN the system SHALL verify whether it is still needed for HTMX requests
3. WHEN a validation function is obsolete THEN the system SHALL remove it
4. WHEN removing validation functions THEN the system SHALL ensure all handlers still validate inputs correctly
5. WHEN validation functions are removed THEN the system SHALL verify all validation tests still pass

### Requirement 6

**User Story:** As a developer, I want to identify and remove unused test files, so that the test suite only contains relevant tests

#### Acceptance Criteria

1. WHEN analyzing test files THEN the system SHALL identify tests for removed functionality
2. WHEN a test for removed functionality is found THEN the system SHALL verify the functionality no longer exists
3. WHEN a test is confirmed obsolete THEN the system SHALL remove it
4. WHEN removing tests THEN the system SHALL ensure the test suite still runs successfully
5. WHEN tests are removed THEN the system SHALL update test documentation

### Requirement 7

**User Story:** As a developer, I want to verify that all remaining code is actively used, so that the codebase contains no dead code

#### Acceptance Criteria

1. WHEN cleanup is complete THEN the system SHALL perform a dead code analysis
2. WHEN dead code is found THEN the system SHALL report the unused functions and files
3. WHEN all cleanup is complete THEN the system SHALL verify the application builds without errors
4. WHEN all cleanup is complete THEN the system SHALL verify all tests pass
5. WHEN all cleanup is complete THEN the system SHALL generate a summary report of removed code
