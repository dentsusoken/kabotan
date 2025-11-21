# Kabotan Specifications

This directory contains all specification documents for the Kabotan project, organized by priority and dependency relationships.

## Specification Structure

Specifications are organized into three tiers based on dependencies and implementation priority:

### Tier 1: Foundation (Infrastructure)
Core infrastructure that all features depend on. Must be implemented first.

- **infra-01-core-infrastructure** - Core LLM integration, language support, and common utilities
- **backend-streaming-02-sse-support** - Streaming LLM responses with SSE
- **backend-llm-03-retry-and-validation** - Logging, loading indicators, conversation history, system prompts
- **frontend-04-htmx-architecture** - HTMX-based frontend with dynamic interactions and streaming UI

### Tier 2: Features (Applications)
Feature implementations that depend on Tier 1 infrastructure.

- **feature-10-character-chat** - Chat with Halloween character personas
- **feature-11-monster-diagnostic** - Monster personality diagnosis
- **feature-12-story-generator** - Halloween story generation
- **feature-13-spell-generator** - Magical phrase generation
- **feature-14-trivia-bot** - Halloween trivia conversations

### Tier 3: Quality & Maintenance (Support)
Code quality, testing, and maintenance tasks.

- **quality-20-code-refactoring** - Code organization and standards compliance
- **quality-testing-21-e2e-playwright** - Mock-based E2E testing infrastructure
- **frontend-22-htmx-refactoring** - HTMX hypermedia-driven architecture refactoring (server-side HTML generation, HTMX SSE extension, session management)
- **quality-23-code-standards-compliance** - Code standards compliance and refactoring
- **quality-24-code-cleanup** - Removal of obsolete code following HTMX migration

## Archived Specifications

The following specifications have been consolidated or superseded:

- `halloween-core/` - Merged into `infra-01-core-infrastructure`
- `halloween-features/` - Split into individual feature specs (feature-10 through feature-14)

## Reading Order

For new developers or comprehensive understanding:

1. Start with `infra-01-core-infrastructure` to understand the foundation
2. Review `backend-streaming-02-sse-support` and `backend-llm-03-retry-and-validation` for infrastructure capabilities
3. Review `frontend-04-htmx-architecture` to understand the original frontend architecture
4. **Review `frontend-22-htmx-refactoring` to understand the current HTMX hypermedia-driven architecture** (most important for current development)
5. Explore individual feature specs (feature-10 through feature-14) based on interest
6. Review quality specs (quality-20, quality-testing-21, quality-23) for maintenance and testing practices

## Current Architecture

**Important**: The project has undergone a significant architectural refactoring (spec frontend-22-htmx-refactoring). The current architecture follows HTMX's hypermedia-driven principles:

- **Server-side HTML generation**: All HTML is generated on the backend
- **HTMX SSE extension**: Streaming uses HTMX's official SSE extension
- **Server-side sessions**: State management happens on the server
- **Minimal JavaScript**: Only language preference management remains
- **HTML responses**: All API endpoints return HTML, not JSON

For the most accurate understanding of the current system, refer to `frontend-22-htmx-refactoring` specification.

## Specification Format

Each specification directory contains:
- `requirements.md` - User stories and acceptance criteria
- `design.md` - Technical design and architecture
- `tasks.md` - Implementation tasks and progress tracking
