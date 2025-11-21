# Specification Dependencies

This document visualizes the dependency relationships between specifications.

## Dependency Graph

```
Tier 1: Foundation (Infrastructure)
┌─────────────────────────────────────────────────────────────┐
│                                                               │
│  infra-01-core-infrastructure (No dependencies)              │
│  └─ Core LLM service, language support, UI framework         │
│                                                               │
└───────────────────┬─────────────────────────────────────────┘
                    │
        ┌───────────┴───────────┬───────────────┐
        │                       │               │
        ▼                       ▼               ▼
┌───────────────────┐   ┌───────────────────┐  │
│                   │   │                   │  │
│ backend-streaming │   │ backend-llm       │  │
│ -02-sse-support   │   │ -03-retry-and     │  │
│                   │   │ -validation       │  │
└─────────┬─────────┘   └─────────┬─────────┘  │
          │                       │             │
          └───────────┬───────────┘             │
                      │                         │
                      ▼                         │
              ┌───────────────────┐             │
              │                   │             │
              │ frontend-04-htmx  │◄────────────┘
              │ -architecture     │
              │   (DEPRECATED)    │
              └─────────┬─────────┘
                        │
          ┌─────────────┴─────────────┐
          │                           │
          ▼                           ▼

Tier 2: Features (Applications)
┌─────────────────────────────────────────────────────────────┐
│                                                               │
│  feature-10-character-chat                                    │
│  └─ Depends on: infra-01, backend-llm-03, frontend-04,       │
│                 frontend-22                                   │
│                                                               │
│  feature-11-monster-diagnostic                                │
│  └─ Depends on: infra-01, frontend-04, frontend-22           │
│                                                               │
│  feature-12-story-generator                                   │
│  └─ Depends on: infra-01, frontend-04, frontend-22           │
│                                                               │
│  feature-13-spell-generator                                   │
│  └─ Depends on: infra-01, frontend-04, frontend-22           │
│                                                               │
│  feature-14-trivia-bot                                        │
│  └─ Depends on: infra-01, backend-llm-03, frontend-04,       │
│                 frontend-22                                   │
│                                                               │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    │ (All features implemented)
                    │
                    ▼

Tier 3: Quality & Maintenance (Support)
┌─────────────────────────────────────────────────────────────┐
│                                                               │
│  quality-20-code-refactoring                                  │
│  └─ Depends on: All implemented features                      │
│                                                               │
│  quality-testing-21-e2e-playwright                            │
│  └─ Depends on: All implemented features                      │
│                                                               │
│  frontend-22-htmx-refactoring (CURRENT ARCHITECTURE)          │
│  └─ Depends on: infra-01, backend-streaming-02,              │
│                 backend-llm-03, frontend-04,                  │
│                 feature-10 through feature-14,                │
│                 quality-20, quality-testing-21                │
│  └─ Supersedes: frontend-04-htmx-architecture                 │
│                                                               │
│  quality-23-code-standards-compliance                         │
│  └─ Depends on: All implemented features                      │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Implementation Order

### Phase 1: Foundation
1. **infra-01-core-infrastructure** - Must be implemented first
2. **backend-streaming-02-sse-support** - Can be implemented after infra-01
3. **backend-llm-03-retry-and-validation** - Can be implemented after infra-01
4. **frontend-04-htmx-architecture** - Can be implemented after infra-01, backend-streaming-02, backend-llm-03

### Phase 2: Features
All features can be implemented in parallel after Phase 1 is complete:
- **feature-10-character-chat** (requires infra-01, backend-llm-03, frontend-04)
- **feature-11-monster-diagnostic** (requires infra-01, frontend-04)
- **feature-12-story-generator** (requires infra-01, frontend-04)
- **feature-13-spell-generator** (requires infra-01, frontend-04)
- **feature-14-trivia-bot** (requires infra-01, backend-llm-03, frontend-04)

### Phase 3: Quality & Maintenance
After features are implemented:
- **quality-20-code-refactoring** - Ongoing maintenance
- **quality-testing-21-e2e-playwright** - Test infrastructure
- **quality-23-code-standards-compliance** - Code standards enforcement

## Dependency Matrix

| Spec                    | infra-01 | backend-02 | backend-03 | frontend-04 | feature-10 | feature-11 | feature-12 | feature-13 | feature-14 | quality-20 | quality-21 | frontend-22 | quality-23 |
|-------------------------|----------|------------|------------|-------------|------------|------------|------------|------------|------------|------------|------------|-------------|------------|
| infra-01                | -        |            |            |             |            |            |            |            |            |            |            |             |            |
| backend-02              | ✓        | -          |            |             |            |            |            |            |            |            |            |             |            |
| backend-03              | ✓        |            | -          |             |            |            |            |            |            |            |            |             |            |
| frontend-04             | ✓        | ✓          | ✓          | -           |            |            |            |            |            |            |            | ⚠           |            |
| feature-10              | ✓        |            | ✓          | ⚠           | -          |            |            |            |            |            |            | ✓           |            |
| feature-11              | ✓        |            |            | ⚠           |            | -          |            |            |            |            |            | ✓           |            |
| feature-12              | ✓        |            |            | ⚠           |            |            | -          |            |            |            |            | ✓           |            |
| feature-13              | ✓        |            |            | ⚠           |            |            |            | -          |            |            |            | ✓           |            |
| feature-14              | ✓        |            | ✓          | ⚠           |            |            |            |            | -          |            |            | ✓           |            |
| quality-20              | ✓        | ✓          | ✓          | ✓           | ✓          | ✓          | ✓          | ✓          | ✓          | -          |            |             |            |
| quality-21              | ✓        | ✓          | ✓          | ✓           | ✓          | ✓          | ✓          | ✓          | ✓          |            | -          |             |            |
| frontend-22             | ✓        | ✓          | ✓          | ✓           | ✓          | ✓          | ✓          | ✓          | ✓          | ✓          | ✓          | -           |            |
| quality-23              | ✓        | ✓          | ✓          | ✓           | ✓          | ✓          | ✓          | ✓          | ✓          | ✓          | ✓          | ✓           | -          |

Legend:
- ✓ = Direct dependency
- ⚠ = Deprecated/Superseded by frontend-22
- Empty = No dependency

Spec abbreviations:
- infra-01 = infra-01-core-infrastructure
- backend-02 = backend-streaming-02-sse-support
- backend-03 = backend-llm-03-retry-and-validation
- frontend-04 = frontend-04-htmx-architecture
- feature-10 through feature-14 = Individual feature implementations
- quality-20 = quality-20-code-refactoring
- quality-21 = quality-testing-21-e2e-playwright
- frontend-22 = frontend-22-htmx-refactoring
- quality-23 = quality-23-code-standards-compliance

## Notes

- **Tier 1** specs provide infrastructure that other specs build upon
- **Tier 2** specs are independent features that can be developed in parallel
- **Tier 3** specs are maintenance tasks that apply to all implemented code
- Streaming support (backend-02) is optional but enhances user experience across all features
- LLM enhancements (backend-03) are required for features that need conversation history or system prompts
- **HTMX frontend (frontend-04) is DEPRECATED** - superseded by frontend-22-htmx-refactoring
- **frontend-22-htmx-refactoring is the CURRENT ARCHITECTURE** - implements hypermedia-driven design with server-side HTML generation, HTMX SSE extension, and session management
- All feature specs (feature-10 through feature-14) now depend on frontend-22 for the current implementation approach
