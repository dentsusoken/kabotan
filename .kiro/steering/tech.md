# Tech Stack

## Backend

### Language & Runtime
- Common Lisp (SBCL implementation)
- Roswell: Common Lisp implementation manager and script launcher

### Core Dependencies
- **ningle**: Lightweight web application framework for API implementation
- **clack**: Web application server interface
- **lack**: Middleware library for Clack
  - lack-middleware-mount: Route mounting
  - lack-middleware-static: Static file serving
- **jonathan**: JSON encoder/decoder for API responses
- **dexador**: HTTP client for making LLM API requests
- **cl-json**: JSON parser for handling API responses
- **cl-ppcre**: Regular expression library for text processing
- **flexi-streams**: Flexible stream handling for SSE
- **fiveam**: Testing framework for backend unit tests

## Frontend

### Core Technologies
- **HTMX 1.9.10**: Hypermedia-driven frontend with declarative HTML attributes
  - Core library for AJAX requests and DOM manipulation
  - SSE Extension for Server-Sent Events streaming
  - Response Targets Extension for error handling
- **TailwindCSS 3.x**: Utility-first CSS framework via CDN
- **DaisyUI 4.4.19**: Component library for Tailwind via CDN
- **Minimal Vanilla JavaScript**: Only essential functionality not provided by HTMX
- HTML served from `/public/` directory (minimal shell, server-driven content)

### JavaScript Modules (Minimal)
- `language-manager.js`: Language preference management (localStorage)
- `page-init.js`: Page initialization and default feature loading
- `streaming-handler.js`: SSE streaming handler for HTMX SSE extension

### HTMX Architecture
- Server returns HTML fragments instead of JSON
- Forms use `hx-post` and `hx-get` attributes for submissions
- Streaming uses HTMX SSE extension with `sse-connect` and `sse-swap` attributes
- Error handling uses Response Targets extension (`hx-target-5*`, `hx-target-4*`)
- Feature switching via `hx-get` requests to `/api/features/*` endpoints

### Testing
- **Playwright 1.40+**: E2E testing framework for frontend testing
- **FiveAM**: Backend unit testing framework

## Build System
- ASDF (Another System Definition Facility) for system definitions
- Makefile for common operations
- Quicklisp for dependency management

## Common Commands

### Development
```bash
# Start REPL with system loaded
make repl

# Run application without debugger
make run

# Run tests
make test

# Clean compiled files
make clean
```

### Direct Roswell Commands
```bash
# Load system and start
ros -L sbcl run -- --eval '(ql:quickload :kabotan)'

# Run main function
ros -L sbcl run -- --disable-debugger --eval '(ql:quickload :kabotan)' --eval '(uiop:quit (kabotan:main))'
```

## Deployment
- Docker-based deployment using Ubuntu 24.04
- GitLab CI/CD pipeline for automated builds
- Container registry: sample-container-registry.example.com/kabotan/kabotan
- Application runs on port binding to 0.0.0.0

## Configuration Notes
- LLM service configured via environment variables:
  - `OPENAI_API_KEY` or `OPENAI_KEY`: API key (default: "dummy")
  - `OPENAI_MODEL`: Model name (default: "gemma3-12b")
  - `OPENAI_HOST`: API endpoint URL (default: "http://localhost:8080/v1/chat/completions")
- Static files served from `/public/` directory
- API routes mounted under `/api` prefix
- Session management:
  - Cookie-based session IDs via Lack session middleware
  - Server-side session storage for language preferences and conversation history
  - Automatic session cleanup for old sessions
