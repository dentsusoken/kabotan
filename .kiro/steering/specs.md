# Specification Guidelines

## Spec Directory Naming Convention

Specification directories in `.kiro/specs/` must follow one of these naming patterns:

### Pattern 1: With Sub-category
```
<category>-<subcategory>-<id>-<description>
```

### Pattern 2: Without Sub-category
```
<category>-<id>-<description>
```

### Categories

Common categories include:
- `frontend` - Frontend-related specifications
- `backend` - Backend-related specifications
- `feature` - Feature implementation specifications
- `infra` - Infrastructure specifications
- `quality` - Code quality and refactoring specifications

### Sub-categories

Sub-categories are optional and used when appropriate:
- `streaming` - Streaming-related functionality
- `llm` - LLM service-related functionality
- `testing` - Testing infrastructure

### ID

- Must be a two-digit number (e.g., `01`, `10`, `24`)
- Should be unique within the category
- Generally assigned sequentially

### Description

- Use kebab-case (lowercase with hyphens)
- Should be concise and descriptive
- Describes the main purpose of the specification

### Examples

Valid spec names:
- `backend-streaming-02-sse-support`
- `backend-llm-03-retry-and-validation`
- `frontend-04-htmx-architecture`
- `feature-10-character-chat`
- `quality-20-code-refactoring`
- `quality-24-htmx-code-cleanup`
- `infra-01-core-infrastructure`

Invalid spec names:
- `code-cleanup-htmx-migration` (missing category and ID)
- `quality-htmx-cleanup` (missing ID)
- `24-quality-cleanup` (wrong order)

## Specification Structure

Each specification directory must contain:
- `requirements.md` - User stories and acceptance criteria (EARS format)
- `design.md` - Technical design and architecture
- `tasks.md` - Implementation tasks and progress tracking

## Specification Dependencies

Specifications are organized into tiers based on dependencies:

### Tier 1: Foundation (Infrastructure)
Core infrastructure that all features depend on. Must be implemented first.

### Tier 2: Features (Applications)
Feature implementations that depend on Tier 1 infrastructure.

### Tier 3: Quality & Maintenance (Support)
Code quality, testing, and maintenance tasks.

See `.kiro/specs/README.md` for the complete dependency structure.
