# 21 - E2E Testing

**Priority:** Tier 3 - Quality & Maintenance  
**Status:** In Progress  
**Dependencies:** All implemented features

## Overview

Implements mock-based E2E testing to reduce test execution time while maintaining test coverage and reliability.

## Key Components

- **Mock Tests**: NO LLM communication, fast execution (1.6-12.2 min)
- **Integration Tests**: Real LLM communication, end-to-end verification (1.5 min)
- Test organization and naming conventions
- Performance optimization

## LLM Communication

- **Mock Tests (default)**: All API calls mocked at Playwright level, NO LLM communication
- **Integration Tests ([INTEGRATION] tag)**: Real LLM API calls for end-to-end verification
- **Verification**: Check application logs for `[LLM-REQUEST]` entries to detect unwanted LLM calls

## Dependencies

- **All feature specs** - E2E tests cover all implemented features

## Dependents

None - This is a testing infrastructure task.

## Files

- `requirements.md` - E2E testing requirements
- `design.md` - Technical design and architecture
- `tasks.md` - Implementation tasks
