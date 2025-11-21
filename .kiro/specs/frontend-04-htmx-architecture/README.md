# Specification 04: HTMX Frontend

## Status: ⚠️ DEPRECATED - Superseded by Spec 22

**This specification has been superseded by [22-htmx-refactoring](../22-htmx-refactoring/README.md).**

The original HTMX frontend implementation described in this specification used a hybrid approach with client-side HTML generation and JSON APIs. This approach has been replaced with a pure hypermedia-driven architecture where:

- Server generates all HTML (no client-side templates)
- HTMX SSE extension handles streaming (no custom EventSource)
- Session-based state management (no client-side storage for history)
- Minimal JavaScript (only essential functionality)

## Historical Context

This specification originally described:

1. **Client-side HTML Templates**: JavaScript files containing HTML templates
2. **JSON API Responses**: Server returned JSON, client rendered HTML
3. **Custom EventSource**: Manual SSE connection management
4. **Client-side State**: Conversation history in sessionStorage
5. **Feature Manager**: JavaScript module for feature switching

## Migration to Spec 22

All functionality from this specification has been reimplemented following hypermedia-driven principles:

- **HTML Templates** → `src/utils/html-templates.lisp` (server-side)
- **JSON APIs** → HTML fragment responses
- **Custom EventSource** → HTMX SSE extension
- **Client-side State** → `src/services/session-manager.lisp` (server-side)
- **Feature Manager** → HTMX `hx-get` attributes

## Current Implementation

For the current architecture and implementation details, refer to:

- [Specification 22: HTMX Refactoring](../22-htmx-refactoring/README.md)
- [Steering: Structure](../../steering/structure.md)
- [Steering: Tech Stack](../../steering/tech.md)

## Original Documentation

The original requirements, design, and tasks are preserved in this directory for historical reference:

- `requirements.md` - Original user stories and acceptance criteria
- `design.md` - Original technical design
- `tasks.md` - Original implementation tasks

**Note**: These documents describe the old architecture and should not be used as a reference for new development.
