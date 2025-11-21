# Design Document

## Overview

This document describes the core infrastructure design for the Halloween-themed web application. This includes the foundational architecture, LLM integration, language support, and common utilities that all Halloween features depend on.

## Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Browser (User)                        │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  HTMX Frontend (public/index.html)                     │ │
│  │  - Language Selector                                   │ │
│  │  - Feature Mode Navigation                             │ │
│  │  - Dynamic Content Areas                               │ │
│  └────────────────────────────────────────────────────────┘ │
└───────────────────────────┬─────────────────────────────────┘
                            │ HTTP/HTMX Requests
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              Kabotan Backend (Common Lisp/Ningle)           │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  API Layer (src/api/)                                  │ │
│  │  - Feature-specific endpoints                          │ │
│  │  - /api/health                                         │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Service Layer (src/services/)                         │ │
│  │  - LLM Service (llm-service.lisp)                      │ │
│  │  - Prompt Builder (prompt-builder.lisp)                │ │
│  │  - Language Handler (language-handler.lisp)            │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Utilities (src/utils/)                                │ │
│  │  - Input Validation                                    │ │
│  │  - Error Handling                                      │ │
│  │  - Response Formatting                                 │ │
│  └────────────────────────────────────────────────────────┘ │
└───────────────────────────┬─────────────────────────────────┘
                            │ HTTP API Calls
                            ▼
┌─────────────────────────────────────────────────────────────┐
│          LLM Service (OpenAI-Compatible API)                │
│          (Configurable via environment variables)           │
└─────────────────────────────────────────────────────────────┘
```

## Technology Stack

- **Backend Framework**: Ningle (lightweight web framework)
- **HTTP Server**: Clack with Lack middleware
- **JSON Processing**: Jonathan
- **LLM Integration**: Dexador HTTP client with cl-json for API communication
- **Frontend**: HTMX for dynamic interactions
- **CSS Framework**: TailwindCSS with DaisyUI components
- **Theme**: Dark mode with Halloween-themed color palette
- **Testing**: FiveAM for backend, Playwright for E2E

## Design Principles

1. **Separation of Concerns**: API layer, service layer, and utilities are clearly separated
2. **Reusability**: Common LLM interaction logic is centralized in service layer
3. **Stateless API**: Each request is independent; session state managed client-side
4. **Progressive Enhancement**: Core functionality works without JavaScript; HTMX enhances UX
5. **Error Resilience**: Graceful degradation when LLM service is unavailable (Requirement 4.5)
6. **Environment-Based Configuration**: LLM settings configurable via environment variables
7. **Dark Mode First**: UI designed for dark mode with Halloween theme
8. **Performance-Oriented**: Fast UI responses (<1 second for navigation, <10 seconds for LLM responses) (Requirement 1.2, 3.2)
9. **Bilingual Support**: Full Japanese and English support with automatic detection (Requirement 2.1-2.5)

## Session and Context Management

### Client-Side State Management
- **Language preference**: Stored in localStorage (Requirement 2.2)
- **Current feature mode**: Managed via HTMX navigation state (Requirement 3.1)
- **Halloween theme context**: Injected into every LLM prompt (Requirement 1.4)

### Context Preservation Strategy
- **Within a session**: Halloween theme context maintained across interactions (Requirement 1.4)
- **Between feature modes**: Context cleared when switching modes (Requirement 3.5)
- **Conversation history**: Managed per-feature by individual feature implementations

### Design Rationale
- **Stateless backend**: Simplifies deployment and scaling
- **Client-side state**: Reduces server load and improves response times
- **Context injection**: Ensures consistent Halloween theming without server-side session management
- **Mode isolation**: Prevents context leakage between different features

## Core Components

### Service Layer

**LLM Service (`src/services/llm-service.lisp`)**
- Core LLM interaction functionality
- Timeout configuration: 30 seconds for LLM API calls (Requirement 1.3)
- Retry logic with exponential backoff for network failures (Requirement 4.4)
- Error handling and logging (Requirement 4.2)
- Response time target: Display results within 10 seconds under normal conditions (Requirement 1.2)

**Language Handler (`src/services/language-handler.lisp`)**
- UI text management for Japanese and English (Requirement 2.1, 2.2)
- Browser language detection from Accept-Language header (Requirement 2.5)
- Translation utilities for dynamic UI updates
- Language context injection for LLM prompts (Requirement 2.3, 2.4)

**Prompt Builder (`src/services/prompt-builder.lisp`)**
- Base prompt construction utilities
- Halloween theme context injection for all interactions (Requirement 1.1, 1.4)
- Language-specific prompt formatting (Requirement 2.3, 2.4)
- Session context preservation mechanisms (Requirement 1.4)

### Utilities

**Validation (`src/utils/validation.lisp`)**
- Input validation functions (Requirement 4.1)
- Empty input detection and validation (Requirement 1.5)
- Language validation (Japanese/English)
- Input sanitization to prevent injection attacks (Requirement 4.3)

**Error Handling (`src/utils/error-handling.lisp`)**
- Error response formatting with user-friendly messages (Requirement 4.2)
- Logging utilities for error tracking (Requirement 4.2)
- Error categorization (validation, LLM service, system errors)
- Graceful error handling to maintain application stability (Requirement 4.5)

**Response Formatting (`src/utils/response-formatting.lisp`)**
- HTML response generation for HTMX integration
- DaisyUI component formatting for consistent styling (Requirement 3.4)
- Dark theme styling with Halloween color palette
- Readable format for Generated Responses (Requirement 3.4)

## Performance Requirements

### Response Time Targets
- **UI Navigation**: Feature mode switching completes within 1 second (Requirement 3.2)
- **LLM Responses**: Display generated responses within 10 seconds under normal conditions (Requirement 1.2)
- **Timeout Threshold**: LLM API calls timeout after 30 seconds (Requirement 1.3)
- **Language Switching**: UI text updates immediately (<100ms) (Requirement 2.2)

### Design Rationale
- **30-second timeout**: Balances user patience with LLM processing time for complex prompts
- **10-second target**: Provides responsive experience while allowing for LLM generation
- **Retry mechanism**: Improves reliability for transient network issues without blocking user
- **Client-side language switching**: Instant updates without server round-trip

## Environment Configuration

### Required Environment Variables
- `OPENAI_API_KEY`: API key for OpenAI-compatible service (default: "dummy")
- `OPENAI_MODEL`: Model name to use (default: "gemma3-12b")
- `OPENAI_HOST`: LLM service endpoint URL (default: "http://localhost:8080/v1/chat/completions")

### Configuration Loading
Configuration is handled directly in the LLM service layer, reading environment variables on each API call for deployment flexibility.

## Frontend Structure

### Main Application Shell (`public/index.html`)
- TailwindCSS and DaisyUI integration via CDN
- Dark mode theme with Halloween color scheme
- Language selector (Japanese/English toggle) (Requirement 2.1, 2.2)
- Feature mode navigation menu (Requirement 3.1)
- Dynamic content container with fast switching (<1 second) (Requirement 3.2)
- Loading indicators for visual feedback during processing (Requirement 3.3)
- Error displays using DaisyUI alert components (Requirement 4.2)
- Context clearing mechanism when switching Feature Modes (Requirement 3.5)

### Language Management
- **Client-side storage**: Language preference stored in localStorage for persistence
- **Automatic detection**: Browser language detected on first visit (Requirement 2.5)
- **Dynamic updates**: UI text updated immediately on language change (Requirement 2.2)
- **API integration**: Language parameter included in all HTMX requests (Requirement 2.3)

### Design Rationale
- **localStorage for language**: Persists user preference across sessions without server-side state
- **HTMX for interactions**: Reduces JavaScript complexity while maintaining dynamic behavior
- **Component-based UI**: DaisyUI components ensure consistent styling and accessibility

## Error Handling Strategy

### Error Categories
1. **Validation Errors**: Invalid or missing input parameters (Requirement 4.1)
   - Empty input prompts (Requirement 1.5)
   - Invalid language selections
   - Malformed requests
2. **LLM Service Errors**: Timeout, connection failure, or API errors (Requirement 1.3, 4.4)
   - 30-second timeout threshold
   - Network failures with retry capability
   - API error responses
3. **System Errors**: Unexpected internal errors (Requirement 4.5)
   - Application maintains stability
   - User can continue using other features

### Error Response Format
All errors returned as HTML fragments using DaisyUI alert component for HTMX integration (Requirement 4.2).

### Design Rationale
- **User-friendly messages**: Technical errors are translated to understandable messages for users
- **Graceful degradation**: Errors in one feature don't affect other features
- **Retry mechanism**: Network failures include retry options to improve reliability (Requirement 4.4)
- **Logging**: All errors are logged for debugging while showing safe messages to users (Requirement 4.2)

## Security Considerations

- Sanitize all user inputs before sending to LLM (Requirement 4.3)
- Validate all request parameters before processing (Requirement 4.1)
- Use HTTPS in production
- Don't expose internal error details to users (Requirement 4.2)
- Rate limit API endpoints to prevent abuse
- Log security-relevant events

### Design Rationale
- **Input sanitization**: Prevents injection attacks by cleaning user input before LLM processing
- **Validation-first approach**: All inputs validated before expensive LLM calls
- **Error message safety**: Internal errors logged but not exposed to prevent information leakage
