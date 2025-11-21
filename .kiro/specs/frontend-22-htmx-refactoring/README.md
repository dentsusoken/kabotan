# Specification 22: HTMX Hypermedia-Driven Architecture Refactoring

## Status: ✅ COMPLETED

This specification documents the architectural refactoring of Kabotan from a JavaScript-heavy, client-side rendering approach to an HTMX-driven, server-side rendering architecture following hypermedia-driven design principles.

## Overview

This refactoring represents a fundamental shift in the application architecture:

- **From**: Client-side HTML generation with JSON APIs
- **To**: Server-side HTML generation with hypermedia responses

The refactoring aligns with HTMX's philosophy of "Locality of Behaviour" where HTML attributes describe interactions directly, and the server is responsible for generating all HTML content.

## Key Changes

### Frontend Architecture

1. **Minimal JavaScript**: Reduced from 8+ JavaScript files to 3 essential modules
   - `language-manager.js`: Language preference management
   - `page-init.js`: Page initialization
   - `streaming-handler.js`: SSE streaming support

2. **HTMX-Driven Interactions**: All dynamic behavior uses HTMX attributes
   - Feature switching via `hx-get` requests
   - Form submissions via `hx-post` requests
   - Streaming via HTMX SSE extension

3. **Server-Driven Content**: All HTML generated on the server
   - Feature forms loaded dynamically
   - Results returned as HTML fragments
   - No client-side HTML templates

### Backend Architecture

1. **HTML Generation Layer**: New `html-templates.lisp` module
   - Functions to generate feature forms
   - HTML fragment formatters for responses
   - DaisyUI component wrappers

2. **Session Management**: New `session-manager.lisp` module
   - Server-side language preference storage
   - Conversation history management
   - Session-based state management

3. **Feature Content Endpoints**: New endpoints for loading features
   - `GET /api/features/*` - Returns HTML forms
   - `POST /api/*` - Returns HTML results
   - `GET /api/*-stream` - Returns SSE with HTML fragments

4. **Modified Handlers**: All handlers now return HTML instead of JSON
   - Content-Type: `text/html; charset=utf-8`
   - HTML escaping for all user content
   - HTMX attributes included in responses

### Streaming Architecture

1. **HTMX SSE Extension**: Replaced custom EventSource implementation
   - Declarative SSE connections via `sse-connect` attribute
   - Automatic content swapping via `sse-swap` attribute
   - Event-based streaming with `message`, `done`, `error` events

2. **HTML Fragment Streaming**: Server sends HTML chunks
   - Each chunk wrapped in appropriate HTML elements
   - Proper escaping for streaming content
   - Session history integration for chat features

## Implementation Status

All 24 tasks completed:

- ✅ HTML template generation infrastructure
- ✅ Session management system
- ✅ Feature content endpoints
- ✅ All 5 features converted to HTML responses
- ✅ Streaming support with HTMX SSE extension
- ✅ Language switching with session management
- ✅ Error handling with response-targets extension
- ✅ E2E tests updated for HTML responses
- ✅ Obsolete JavaScript files removed
- ✅ Documentation updated

## Architecture Benefits

1. **Simplicity**: Reduced client-side complexity significantly
2. **Maintainability**: Single source of truth for HTML generation
3. **Locality of Behaviour**: HTML attributes describe interactions directly
4. **Performance**: Smaller JavaScript bundle, better caching
5. **Security**: Server-side validation and HTML escaping
6. **Testability**: Easier to test HTML responses

## Migration Impact

### Removed Files
- `public/js/feature-content-templates.js`
- `public/js/feature-ui-handlers.js`
- `public/js/feature-streaming-handlers.js`
- `public/js/streaming-manager.js`
- `public/js/chat-manager.js`
- `public/js/feature-manager.js`
- `public/js/chat-streaming.js`
- `public/js/chat-history.js`
- `public/js/error-handler.js`
- `public/js/app-init.js`

### Added Files
- `src/utils/html-templates.lisp`
- `src/services/session-manager.lisp`
- `src/api/handlers/feature-content-handler.lisp`
- `public/js/page-init.js`
- `public/js/streaming-handler.js`

### Modified Files
- `public/index.html` - Minimal shell with HTMX extensions
- `public/js/language-manager.js` - Simplified to localStorage management
- All handler files - Return HTML instead of JSON
- `src/main.lisp` - Added session middleware

## Testing

All E2E tests updated and passing:
- Feature loading tests
- Form submission tests
- Streaming tests
- Language switching tests
- Error handling tests

Backend unit tests maintained:
- HTML generation tests
- Session management tests
- Handler response format tests

## Future Enhancements

Potential improvements identified but not implemented:

1. **Template Engine**: Consider using CL-WHO or Djula for more complex templates
2. **Component Library**: Build reusable HTML component functions
3. **Progressive Enhancement**: Add optional JavaScript enhancements
4. **Performance Optimization**: Template caching, session cleanup scheduler
5. **Security Hardening**: CSRF tokens, CSP headers, rate limiting

## Related Specifications

- **Supersedes**: 04-htmx-frontend (original HTMX implementation)
- **Depends on**: 01-core-infrastructure, 02-streaming-support, 03-llm-enhancements
- **Affects**: All feature specs (10-14)

## Documentation

- `requirements.md` - User stories and acceptance criteria
- `design.md` - Technical design and architecture details
- `tasks.md` - Implementation tasks and progress tracking

## Notes

This refactoring represents the **current architecture** of the Kabotan application. All new development should follow the patterns established in this specification:

- Server generates HTML, not JSON
- HTMX attributes for interactions
- Session-based state management
- Minimal client-side JavaScript
- HTMX SSE extension for streaming

For detailed implementation guidance, refer to the steering files in `.kiro/steering/`.
