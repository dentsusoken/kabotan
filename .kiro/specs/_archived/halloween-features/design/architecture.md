# Architecture

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Browser (User)                        │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  HTMX Frontend (public/index.html + components)        │ │
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
│  │  - /api/monster-diagnostic                             │ │
│  │  - /api/story-generator                                │ │
│  │  - /api/character-chat                                 │ │
│  │  - /api/trivia-bot                                     │ │
│  │  - /api/daily-spell                                    │ │
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
- **Templating**: Server-side HTML generation with Common Lisp
- **Testing**: FiveAM for backend, Playwright for E2E

## Design Principles

1. **Separation of Concerns**: API layer, service layer, and utilities are clearly separated
2. **Reusability**: Common LLM interaction logic is centralized in service layer
3. **Stateless API**: Each request is independent; session state managed client-side or via cookies
4. **Progressive Enhancement**: Core functionality works without JavaScript; HTMX enhances UX
5. **Error Resilience**: Graceful degradation when LLM service is unavailable
6. **Environment-Based Configuration**: LLM settings configurable via environment variables
7. **Dark Mode First**: UI designed for dark mode with Halloween theme
