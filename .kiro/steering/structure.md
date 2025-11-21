# Project Structure

## Root Files
- `kabotan.asd`: Main ASDF system definition
- `kabotan-test.asd`: Test system definition
- `Makefile`: Build and run commands
- `Dockerfile`: Container image definition
- `.gitlab-ci.yml`: CI/CD pipeline configuration
- `playwright.config.js`: Playwright E2E test configuration

## Source Organization

### `/src/`
Main application source code organized in layers:
- `package.lisp`: Package definition and exports (must be loaded first)
- `main.lisp`: Application entry point and server setup with Lack middleware stack

#### `/src/utils/`
Utility functions and common helpers organized by responsibility:

**Validation and Error Handling:**
- `validation.lisp`: Input validation functions
- `error-handling.lisp`: Error handling and logging

**HTML Generation (split by component type):**
- `html-common.lisp`: HTML escaping, sanitization, and basic DaisyUI components (cards, alerts, containers)
- `html-forms.lisp`: Form input components (inputs, textareas, selects, buttons, form wrappers)
- `html-features.lisp`: Feature-specific form generators (spell, monster, story generators)
- `html-chat.lisp`: Chat and conversation UI components (character chat, trivia bot)

**Streaming (split by protocol layer):**
- `sse-protocol.lisp`: SSE protocol utilities (parsing, formatting, response creation)
- `streaming-handlers.lisp`: HTMX streaming handlers and error handling
- `streaming-error-handler.lisp`: Streaming-specific error handling utilities

**Other Utilities:**
- `logging.lisp`: LLM request/response logging
- `handler-utils.lisp`: Common handler utilities
- `response-formatting.lisp`: Response formatting utilities

#### `/src/services/`
Business logic and external service integration organized by responsibility:

**LLM Service (split by functionality):**
- `llm-client.lisp`: Core LLM API client functions and configuration
- `llm-retry.lisp`: Retry logic wrappers and message validation
- `llm-streaming.lisp`: Streaming API functions

**Other Services:**
- `language-handler.lisp`: Multi-language support
- `prompt-builder.lisp`: LLM prompt construction
- `session-manager.lisp`: Server-side session management (language preferences, conversation history)

#### `/src/api/`
API layer and route handlers:
- `halloween-api.lisp`: API route setup and configuration

#### `/src/api/handlers/`
Individual feature handlers (return HTML fragments for HTMX):
- `monster-diagnostic-handler.lisp`: Monster personality diagnosis
- `story-generator-handler.lisp`: Halloween story generation
- `character-chat-handler.lisp`: Character chat with session-based history
- `trivia-bot-handler.lisp`: Halloween trivia conversations with session-based history
- `spell-generator-handler.lisp`: Daily spell generation
- `feature-content-handler.lisp`: Feature form generation and language switching

### `/tests/`
Test suite using FiveAM framework, organized by functional area:

**Core Test Infrastructure:**
- `package.lisp`: Test package definition
- `tests.lisp`: Main test runner

**Validation and Error Handling Tests:**
- `validation-tests.lisp`: Input validation tests
- `error-handling-tests.lisp`: Error handling tests

**Service Tests:**
- `llm-service-tests.lisp`: LLM service tests (client, retry, messages)
- `language-handler-tests.lisp`: Language handler tests
- `prompt-builder-tests.lisp`: Prompt builder tests
- `session-manager-tests.lisp`: Session management tests

**Utility Tests:**
- `response-formatting-tests.lisp`: Response formatting tests
- `logging-tests.lisp`: Logging functionality tests

**HTML Template Tests:**
- `html-common-tests.lisp`: HTML escaping and common component tests
- `html-forms-tests.lisp`: Form component tests
- `html-features-tests.lisp`: Feature-specific form and chat component tests

**Streaming Tests:**
- `streaming-tests.lisp`: SSE protocol and streaming handler tests
- `streaming-llm-test.lisp`: Streaming LLM integration tests
- `manual-streaming-test.lisp`: Manual streaming tests (for development)
- `manual-streaming-llm-test.lisp`: Manual LLM streaming tests (for development)

**API Tests:**
- `message-api-tests.lisp`: Message API and validation tests
- `feature-content-tests.lisp`: Feature content handler tests

**Property-Based Tests:**
- `property-test-utils.lisp`: Property-based testing utilities
- `property-behavior-preservation-tests.lisp`: Behavior preservation properties
- `property-test-coverage-tests.lisp`: Test coverage maintenance properties
- `property-export-preservation-tests.lisp`: Export preservation properties

### `/e2e-tests/`
End-to-end tests using Playwright:
- `README.md`: E2E testing documentation
- `*.spec.js`: Feature-specific E2E test suites
- `playwright.config.js`: Playwright configuration

### `/public/`
Static assets served by the web application:
- `index.html`: Main HTML shell with HTMX (minimal, server-driven content)
- `custom-styles.css`: Custom Halloween theme styles

#### `/public/js/`
Minimal frontend JavaScript modules (HTMX-driven architecture):
- `language-manager.js`: Language preference management (localStorage)
- `page-init.js`: Page initialization and default feature loading
- `streaming-handler.js`: SSE streaming handler for HTMX SSE extension

## Code Organization Patterns

### Package Structure
- Main package: `:kabotan`
- Test package: `:kabotan.tests`
- Exports only public API (`:main` function)

### Loading Order
Files must be loaded in serial order as specified in `.asd` files:

1. **Package Definition:**
   - `package.lisp` (defines namespace and exports)

2. **Utilities Layer (with internal dependencies):**
   - `utils/validation.lisp`
   - `utils/error-handling.lisp`
   - `utils/logging.lisp`
   - `utils/html-common.lisp` (base HTML utilities)
   - `utils/html-forms.lisp` (depends on html-common)
   - `utils/html-features.lisp` (depends on html-common and html-forms)
   - `utils/html-chat.lisp` (depends on html-common and html-forms)
   - `utils/response-formatting.lisp`
   - `utils/sse-protocol.lisp` (SSE protocol layer)
   - `utils/streaming-error-handler.lisp`
   - `utils/streaming-handlers.lisp` (depends on sse-protocol)
   - `utils/handler-utils.lisp`

3. **Services Layer (with internal dependencies):**
   - `services/language-handler.lisp`
   - `services/prompt-builder.lisp`
   - `services/session-manager.lisp`
   - `services/llm-client.lisp` (core LLM client)
   - `services/llm-retry.lisp` (depends on llm-client)
   - `services/llm-streaming.lisp` (depends on llm-client)

4. **API Handlers:**
   - `api/handlers/*.lisp` (all feature handlers)

5. **API Layer:**
   - `api/halloween-api.lisp` (route setup)

6. **Main Application:**
   - `main.lisp` (entry point and server setup)

**Dependency Rationale:**
- HTML modules follow a hierarchy: common → forms → features/chat
- LLM modules follow a hierarchy: client → retry/streaming
- Streaming modules follow a hierarchy: protocol → handlers
- This ensures that base functionality is available before dependent modules load

### Route Organization

#### Feature Content Routes (GET - returns HTML forms)
- `/api/features/monster-diagnostic` - Monster diagnostic form
- `/api/features/story-generator` - Story generator form
- `/api/features/character-chat` - Character chat interface with history
- `/api/features/trivia-bot` - Trivia bot interface with history
- `/api/features/spell-generator` - Spell generator form

#### Feature Processing Routes (POST - returns HTML fragments)
- `/api/monster-diagnostic` - Process monster diagnostic (returns HTML result)
- `/api/story-generator` - Generate story (returns HTML result)
- `/api/character-chat` - Process chat message (returns HTML with history)
- `/api/trivia-bot` - Process trivia question (returns HTML with history)
- `/api/spell-generator` - Generate spell (returns HTML result)

#### Streaming Routes (GET with SSE - returns HTML fragments as events)
- `/api/monster-diagnostic-stream` - Streaming monster diagnosis
- `/api/story-generator-stream` - Streaming story generation
- `/api/character-chat-stream` - Streaming character chat
- `/api/trivia-bot-stream` - Streaming trivia responses
- `/api/spell-generator-stream` - Streaming spell generation

#### Utility Routes
- `/api/set-language` - Set session language preference (POST)
- `/api/health` - Health check endpoint

#### Static Routes
- `/` - Serves `index.html` (minimal shell)
- `/static/*` - Static files (CSS, JS, images)
- 404 handling for unmatched routes

### Application Structure
- API routes defined in separate Ningle app instance
- Middleware stack built with Lack builder:
  - Session middleware for server-side state management
  - Custom error handler to suppress 404 stack traces
  - Static file middleware for `/static/` path
  - API routes mounted under `/api` prefix
  - Access logging enabled
- HTMX-driven architecture: server returns HTML fragments, not JSON
- Session-based state management for language preferences and conversation history


## Module Dependencies

### HTML Template Modules
```
html-common.lisp (base)
    ↓
html-forms.lisp
    ↓
html-features.lisp
html-chat.lisp
```

**html-common.lisp** provides:
- HTML escaping and sanitization functions
- Basic DaisyUI components (cards, alerts, containers)

**html-forms.lisp** provides:
- Form input components (inputs, textareas, selects)
- Form wrappers and buttons
- Depends on html-common for escaping

**html-features.lisp** provides:
- Feature-specific form generators (spell, monster, story)
- Depends on html-common and html-forms

**html-chat.lisp** provides:
- Chat and conversation UI components
- Character chat and trivia bot forms
- Depends on html-common and html-forms

### LLM Service Modules
```
llm-client.lisp (base)
    ↓
llm-retry.lisp
llm-streaming.lisp
```

**llm-client.lisp** provides:
- Core API client functions
- Configuration variables
- Test override variables

**llm-retry.lisp** provides:
- Retry logic wrappers
- Message validation
- Depends on llm-client

**llm-streaming.lisp** provides:
- Streaming API functions
- Depends on llm-client

### Streaming Modules
```
sse-protocol.lisp (base)
    ↓
streaming-handlers.lisp
```

**sse-protocol.lisp** provides:
- SSE protocol utilities (parsing, formatting)
- SSE response creation

**streaming-handlers.lisp** provides:
- HTMX streaming handlers
- Error handling for streaming
- Depends on sse-protocol and LLM service modules

## Guidelines for Adding New Functionality

### Adding New HTML Components
1. **Common components** (buttons, cards, etc.) → Add to `html-common.lisp`
2. **Form components** (inputs, selects, etc.) → Add to `html-forms.lisp`
3. **Feature-specific forms** → Add to `html-features.lisp`
4. **Chat/conversation components** → Add to `html-chat.lisp`

### Adding New LLM Functionality
1. **Core API changes** → Modify `llm-client.lisp`
2. **Retry logic changes** → Modify `llm-retry.lisp`
3. **Streaming changes** → Modify `llm-streaming.lisp`

### Adding New Streaming Features
1. **Protocol-level changes** → Modify `sse-protocol.lisp`
2. **Handler-level changes** → Modify `streaming-handlers.lisp`

### Adding New Tests
1. **Validation tests** → Add to `validation-tests.lisp`
2. **Service tests** → Add to appropriate service test file
3. **HTML tests** → Add to appropriate HTML test file
4. **Streaming tests** → Add to `streaming-tests.lisp`
5. **Property-based tests** → Add to appropriate property test file

### File Size Guidelines
- Keep files under 500 lines
- If a file exceeds 500 lines, consider splitting by:
  - Functional area (e.g., separate validation types)
  - Component type (e.g., separate form types)
  - Protocol layer (e.g., separate protocol from handlers)

### Function Size Guidelines
- Keep functions under 100 lines
- If a function exceeds 100 lines, consider:
  - Extracting helper functions
  - Separating concerns (e.g., validation, processing, formatting)
  - Breaking down complex logic into smaller steps
- Exception: Functions with extensive error handling or documentation may exceed 100 lines if splitting would reduce clarity

### Nesting Depth Guidelines
- Keep nesting depth at 6 levels or less
- If nesting exceeds 6 levels, consider:
  - Extracting nested logic into helper functions
  - Using early returns to reduce nesting
  - Simplifying conditional logic
