# Kabotan

A Halloween-themed web application built with Common Lisp that provides interactive LLM-powered features including character chat, story generation, spell creation, monster diagnostics, and trivia games.

## Features

- **Character Chat**: Converse with Halloween characters (Dracula, Witch, Frankenstein, Ghost)
- **Story Generator**: Generate personalized Halloween stories
- **Spell Generator**: Create magical spells with mystical descriptions
- **Monster Diagnostic**: Get humorous diagnoses for your "monster symptoms"
- **Trivia Bot**: Test your Halloween knowledge with an interactive quiz
- **Multi-language Support**: Available in English and Japanese
- **Streaming Responses**: Real-time progressive display of LLM responses (optional)

## Running the Application

### Configuration

#### Environment Variables

- `OPENAI_API_KEY` or `OPENAI_KEY`: API key for OpenAI-compatible service (default: "dummy")
- `OPENAI_MODEL`: Model name to use (default: "gpt-oss-120b")
- `OPENAI_HOST`: API endpoint URL (default: "http://localhost:8080/v1/chat/completions")

#### Example Configuration

```bash
# Basic configuration
export OPENAI_API_KEY="your-api-key"
export OPENAI_MODEL="gpt-4"
export OPENAI_HOST="https://api.openai.com/v1/chat/completions"
```

### Run with local environment

Start the application with automatic reloading:
```bash
make run
```

The application will be available at `http://localhost:5000`

### Run with Docker(Recommend)

Run with Docker:
```bash
docker build -t kabotan .
docker run -p 5000:5000 \
  -e OPENAI_HOST="http://localhost:8080/v1/chat/completions" \
  -e OPENAI_MODEL="gpt-oss-120b" \
  -e OPENAI_API_KEY="dummy" \
  kabotan
```

The application will be available at `http://localhost:5000`

## Architecture

Kabotan follows a **hypermedia-driven architecture** using HTMX, where the server generates HTML responses and the client uses declarative HTML attributes for interactions. This approach provides:

- **Server-side rendering**: All HTML generation happens on the backend
- **Declarative interactions**: HTMX attributes replace imperative JavaScript
- **Simplified client**: Minimal JavaScript for essential functionality only
- **Locality of Behaviour**: HTML attributes describe behavior directly
- **Better maintainability**: Single source of truth for rendering logic

### Request Flow

```
User clicks tab
  → HTMX sends GET request
  → Server generates HTML form
  → HTMX swaps HTML into DOM
  → User submits form
  → HTMX sends POST request
  → Server generates HTML response
  → HTMX swaps HTML into DOM
```

### Streaming Flow

```
User submits form
  → HTMX establishes SSE connection (via SSE extension)
  → Server sends SSE events with HTML fragments
  → HTMX swaps HTML fragments into DOM
  → Server sends completion event
  → HTMX closes connection automatically
```

## Tech Stack

### Backend
- Common Lisp (SBCL)
- Ningle: Web framework
- Clack: HTTP server
- Dexador: HTTP client for LLM API calls
- FiveAM: Testing framework

### Frontend
- **HTMX 1.9.10**: Core library for dynamic interactions
- **HTMX SSE Extension**: Server-Sent Events support for streaming
- **HTMX Response-Targets Extension**: Error handling with targeted responses
- **TailwindCSS 3.x**: Utility-first CSS framework
- **DaisyUI 4.4.19**: Component library for Tailwind
- **Vanilla JavaScript**: Minimal JavaScript for language preference management

## Prerequisites

- [Roswell](https://github.com/roswell/roswell): Common Lisp implementation manager
- [Node.js](https://nodejs.org/): For E2E testing (optional)
- [Playwright](https://playwright.dev/): For E2E testing (optional)

## Installation (for Development)

1. Clone the repository:
```bash
git clone <repository-url>
cd kabotan
```

2. Install SBCL and Setup ASDF source registry:

```bash
# Install SBCL
ros install sbcl

# Setup ASDF
mkdir -p ~/.config/common-lisp/source-registry.conf.d 
echo '(:tree "<path-of-kabotan-source")' > ~/.config/common-lisp/source-registry.conf.d/10-kabotan.conf
```

3. (Optional) Install Node.js dependencies for E2E testing:
```bash
npm install
```

## Testing

### Backend Unit Tests

Run Common Lisp unit tests:
```bash
export OPENAI_API_KEY="dummy"
export OPENAI_MODEL="gpt-oss-120b"
export OPENAI_HOST="http://localhost:8080/v1/chat/completions"
make test
```

### E2E Tests

Run Playwright E2E tests:
```bash
export OPENAI_API_KEY="dummy"
export OPENAI_MODEL="gpt-oss-120b"
export OPENAI_HOST="http://localhost:8080/v1/chat/completions"

# Run all E2E tests
npx playwright test --reporter=line --timeout=120000

# Run specific test file
npx playwright test e2e-tests/character-chat.spec.js --reporter=line --timeout=120000

# Run tests matching a pattern
npx playwright test --grep "streaming" --reporter=line --timeout=120000
```

**Note**: The application must be running on port 5000 before running E2E tests.

## HTMX Integration

### HTMX Attributes

Kabotan uses HTMX attributes to create dynamic interactions without writing JavaScript:

#### Core Attributes
- `hx-get`: Load feature content from server
- `hx-post`: Submit forms and send data
- `hx-target`: Specify where to swap content (e.g., `#feature-content`)
- `hx-swap`: Control swap strategy (`innerHTML`, `outerHTML`, `beforeend`)
- `hx-vals`: Add dynamic values to requests (e.g., language preference)

#### Extensions
- `hx-ext="sse"`: Enable Server-Sent Events extension
- `hx-ext="response-targets"`: Enable error handling with targeted responses
- `sse-connect`: Establish SSE connection to streaming endpoint
- `sse-swap`: Specify which SSE event to listen for and swap

#### Error Handling
- `hx-target-5*`: Target for 5xx server errors
- `hx-target-4*`: Target for 4xx client errors

### Example: Feature Tab

```html
<button hx-get="/api/features/monster-diagnostic"
        hx-target="#feature-content"
        hx-swap="innerHTML">
  Monster Diagnostic
</button>
```

### Example: Form Submission

```html
<form hx-post="/api/monster-diagnostic"
      hx-target="#result-container"
      hx-swap="innerHTML"
      hx-target-5*="#error-display">
  <input type="text" name="favorite_food" required />
  <button type="submit">Diagnose</button>
</form>
```

### Example: Streaming with SSE Extension

```html
<form hx-ext="sse"
      sse-connect="/api/character-chat-stream"
      sse-swap="message"
      hx-target="#chat-container"
      hx-swap="beforeend">
  <input type="text" name="message" required />
  <button type="submit">Send</button>
</form>
```

## Session Management

### Server-Side Sessions

Kabotan uses server-side session management to maintain state across requests:

- **Session ID**: Generated on first request, stored in cookie
- **Language Preference**: Stored in session, persists across page loads
- **Conversation History**: Maintained server-side for character-chat and trivia-bot
- **Session Cleanup**: Automatic cleanup of old sessions (24-hour expiration)

### Session Data Structure

```lisp
(defstruct session
  (id nil :type string)
  (language "ja" :type string)
  (created-at (get-universal-time) :type integer)
  (last-accessed (get-universal-time) :type integer)
  (conversations (make-hash-table :test 'equal) :type hash-table))
```

### Language Switching

Language preference is managed through sessions:

1. User toggles language selector
2. HTMX sends POST request to `/api/set-language`
3. Server updates session language preference
4. Server returns updated feature content in new language
5. HTMX swaps content into DOM

## Streaming Feature

### Overview

The streaming feature enables real-time progressive display of LLM responses using Server-Sent Events (SSE) via the HTMX SSE extension. Instead of waiting for the complete response, users see text appear as it's generated, providing a more responsive and engaging experience.

### How It Works

1. User submits a request (e.g., chat message, story prompt)
2. HTMX SSE extension establishes SSE connection to streaming endpoint
3. Backend streams HTML fragments as SSE events
4. HTMX automatically swaps HTML fragments into target element
5. Server sends completion event ("done")
6. HTMX closes connection automatically

### SSE Event Types

The backend sends different event types for different purposes:

- `message`: Content chunks (HTML fragments)
- `done`: Stream completion signal
- `error`: Error messages (HTML error fragments)

### Streaming Endpoints

When streaming is enabled, the following endpoints are available:

- `GET /api/character-chat-stream`: Streaming character chat
- `GET /api/story-generator-stream`: Streaming story generation
- `GET /api/spell-generator-stream`: Streaming spell creation
- `GET /api/monster-diagnostic-stream`: Streaming monster diagnosis
- `GET /api/trivia-bot-stream`: Streaming trivia responses

### HTML Fragment Streaming

Unlike traditional JSON streaming, Kabotan streams HTML fragments:

```lisp
;; Backend sends HTML fragments as SSE events
(funcall writer (format-sse-event 
                 "message"
                 "<span class=\"chat-bubble\">Hello</span>"))

;; On completion
(funcall writer (format-sse-event "done" ""))
```

### Browser Compatibility

Streaming requires browser support for the EventSource API (Server-Sent Events):
- ✅ Chrome/Edge 6+
- ✅ Firefox 6+
- ✅ Safari 5+
- ✅ Opera 11+

The application automatically falls back to non-streaming mode for unsupported browsers.

### Performance Considerations

- **DOM Update Throttling**: Updates are throttled to maximum 10 per second to maintain browser responsiveness
- **Efficient Buffering**: Chunks are buffered and batched for optimal performance
- **Memory Management**: Resources are properly cleaned up when streams close
- **Timeout Handling**: Streams automatically timeout after configured duration

## Troubleshooting

### General Issues

#### Application Won't Start

**Symptoms**: Application fails to start or crashes immediately

**Solutions**:
1. Check all dependencies are installed: `ros install`
2. Verify port 5000 is not in use: `lsof -i :5000`
3. Review error messages in console
4. Try cleaning and rebuilding: `make clean && make run`
5. Ensure Roswell is properly installed: `ros version`

#### Tests Failing

**Symptoms**: Unit tests or E2E tests fail

**Solutions**:
1. Ensure application is running on port 5000 for E2E tests
2. Check that LLM service is accessible
3. Review test output for specific failures
4. Verify environment variables are set correctly
5. Check network connectivity to LLM service

### Debug Mode

To enable verbose logging for troubleshooting:

```bash
# Backend logging
export LOG_LEVEL="DEBUG"
make run

# Frontend debugging
# Open browser console (F12) and check for errors
```

## API Endpoints

### Feature Content Endpoints (GET)

Load feature forms with HTMX attributes:

- `GET /api/features/monster-diagnostic` - Returns HTML form for monster diagnostic
- `GET /api/features/story-generator` - Returns HTML form for story generation
- `GET /api/features/character-chat` - Returns HTML chat interface with history
- `GET /api/features/trivia-bot` - Returns HTML trivia interface with history
- `GET /api/features/spell-generator` - Returns HTML form for spell generation

**Query Parameters:**
- `language`: Language preference (`ja` or `en`)

**Response:** HTML fragment ready for DOM insertion

### Feature Submission Endpoints (POST)

Submit forms and receive HTML responses:

- `POST /api/monster-diagnostic` - Process monster diagnostic, return HTML result
- `POST /api/story-generator` - Generate story, return HTML result
- `POST /api/character-chat` - Process chat message, return HTML with history
- `POST /api/trivia-bot` - Process trivia question, return HTML with history
- `POST /api/spell-generator` - Generate spell, return HTML result

**Request:** Form data (application/x-www-form-urlencoded)

**Response:** HTML fragment with result and optional form for next interaction

### Streaming Endpoints (GET with SSE)

Stream HTML fragments in real-time:

- `GET /api/character-chat-stream` - Stream chat response as HTML fragments
- `GET /api/story-generator-stream` - Stream story as HTML fragments
- `GET /api/spell-generator-stream` - Stream spell as HTML fragments
- `GET /api/monster-diagnostic-stream` - Stream diagnosis as HTML fragments
- `GET /api/trivia-bot-stream` - Stream trivia response as HTML fragments

**Query Parameters:** Feature-specific parameters (e.g., `message`, `character`, `language`)

**Response:** Server-Sent Events with HTML fragments

**SSE Event Format:**
```
event: message
data: <span class="text">Content chunk</span>

event: done
data: 
```

### Language Management

- `POST /api/set-language` - Update session language preference

**Request Body:**
```json
{
  "language": "en"
}
```

**Response:** Updated feature content HTML in new language

## Project Structure

```
kabotan/
├── src/
│   ├── package.lisp              # Package definitions
│   ├── main.lisp                 # Application entry point
│   ├── api/
│   │   ├── halloween-api.lisp    # API routes
│   │   └── handlers/
│   │       ├── feature-content-handler.lisp  # Feature loading & language
│   │       ├── character-chat-handler.lisp   # Character chat
│   │       ├── story-generator-handler.lisp  # Story generation
│   │       ├── spell-generator-handler.lisp  # Spell generation
│   │       ├── monster-diagnostic-handler.lisp # Monster diagnostic
│   │       └── trivia-bot-handler.lisp       # Trivia bot
│   ├── services/
│   │   ├── llm-service.lisp      # LLM integration
│   │   ├── language-handler.lisp # i18n support
│   │   ├── prompt-builder.lisp   # Prompt construction
│   │   └── session-manager.lisp  # Session management
│   └── utils/
│       ├── html-templates.lisp   # HTML generation
│       ├── streaming.lisp        # SSE utilities
│       ├── streaming-error-handler.lisp # Streaming error handling
│       ├── error-handling.lisp   # Error handling
│       ├── response-formatting.lisp # HTML response formatting
│       ├── handler-utils.lisp    # Common handler utilities
│       ├── validation.lisp       # Input validation
│       └── logging.lisp          # Request/response logging
├── public/
│   ├── index.html                # Main HTML shell
│   ├── custom-styles.css         # Custom styles
│   └── js/
│       └── language-manager.js   # Language preference management
├── tests/
│   ├── package.lisp              # Test package definition
│   ├── tests.lisp                # Main test runner
│   ├── html-templates-tests.lisp # HTML generation tests
│   ├── session-manager-tests.lisp # Session management tests
│   ├── feature-content-tests.lisp # Feature content tests
│   └── *.lisp                    # Other unit tests
├── e2e-tests/
│   ├── character-chat.spec.js    # Character chat E2E tests
│   ├── story-generator.spec.js   # Story generator E2E tests
│   ├── spell-generator.spec.js   # Spell generator E2E tests
│   ├── monster-diagnostic.spec.js # Monster diagnostic E2E tests
│   ├── trivia-bot.spec.js        # Trivia bot E2E tests
│   ├── language-switching.spec.js # Language switching E2E tests
│   ├── streaming-*.spec.js       # Streaming E2E tests
│   └── error-handling.spec.js    # Error handling E2E tests
├── kabotan.asd                   # System definition
├── kabotan-test.asd              # Test system definition
├── Makefile                      # Build commands
├── Dockerfile                    # Container image definition
├── playwright.config.js          # Playwright configuration
└── README.md                     # This file
```

## Development

### Adding New Features

1. **Create HTML Template Generator** in `src/utils/html-templates.lisp`:
```lisp
(defun generate-new-feature-form (language)
  "Generate HTML form for new feature"
  (format nil "<form hx-post=\"/api/new-feature\" 
                     hx-target=\"#result-container\"
                     hx-swap=\"innerHTML\">
                <input type=\"text\" name=\"input\" required />
                <button type=\"submit\">~A</button>
              </form>"
          (get-translation "submit" language)))
```

2. **Create Handler** in `src/api/handlers/new-feature-handler.lisp`:
```lisp
(defun handle-new-feature-request (params)
  "Handle new feature request and return HTML"
  (let* ((session-id (get-session-id params))
         (language (get-session-language session-id))
         (input (cdr (assoc "input" params :test #'string=)))
         (result (process-new-feature input))
         (html (format-new-feature-result result language)))
    `(200 (:content-type "text/html; charset=utf-8")
      (,html))))
```

3. **Add Route** in `src/api/halloween-api.lisp`:
```lisp
(setf (ningle:route *api-app* "/api/features/new-feature" :method :GET)
      #'handle-get-new-feature)
(setf (ningle:route *api-app* "/api/new-feature" :method :POST)
      #'handle-new-feature-request)
```

4. **Add Prompt Builder** in `src/services/prompt-builder.lisp`:
```lisp
(defun build-new-feature-prompt (input language)
  "Build prompt for new feature"
  (format nil "Process this input: ~A" input))
```

5. **Update index.html** to add feature tab:
```html
<button hx-get="/api/features/new-feature"
        hx-target="#feature-content"
        hx-swap="innerHTML">
  New Feature
</button>
```

6. **Add Tests**:
   - Backend unit tests in `tests/new-feature-tests.lisp`
   - E2E tests in `e2e-tests/new-feature.spec.js`

### HTMX Development Patterns

#### Pattern 1: Simple Form Submission
```html
<form hx-post="/api/endpoint"
      hx-target="#result"
      hx-swap="innerHTML">
  <input type="text" name="field" />
  <button type="submit">Submit</button>
</form>
```

#### Pattern 2: Streaming with SSE
```html
<form hx-ext="sse"
      sse-connect="/api/endpoint-stream?param=value"
      sse-swap="message"
      hx-target="#result"
      hx-swap="beforeend">
  <button type="submit">Start Stream</button>
</form>
```

#### Pattern 3: Error Handling
```html
<form hx-post="/api/endpoint"
      hx-target="#result"
      hx-target-5*="#error-display"
      hx-target-4*="#error-display">
  <input type="text" name="field" />
  <button type="submit">Submit</button>
</form>
<div id="error-display"></div>
```

#### Pattern 4: Loading Indicators
```html
<form hx-post="/api/endpoint"
      hx-target="#result"
      hx-indicator="#loading">
  <input type="text" name="field" />
  <button type="submit">Submit</button>
  <span id="loading" class="htmx-indicator loading loading-spinner"></span>
</form>
```

### HTML Generation Best Practices

1. **Always escape user input**:
```lisp
(defun escape-html (text)
  "Escape HTML special characters"
  (let ((text (or text "")))
    (setf text (cl-ppcre:regex-replace-all "&" text "&amp;"))
    (setf text (cl-ppcre:regex-replace-all "<" text "&lt;"))
    (setf text (cl-ppcre:regex-replace-all ">" text "&gt;"))
    (setf text (cl-ppcre:regex-replace-all "\"" text "&quot;"))
    text))
```

2. **Include HTMX attributes in responses**:
```lisp
(defun format-result-with-form (result language)
  "Return result HTML with form for next interaction"
  (format nil "<div class=\"result\">~A</div>
               <form hx-post=\"/api/endpoint\"
                     hx-target=\"#result\">
                 <input type=\"text\" name=\"field\" />
                 <button type=\"submit\">~A</button>
               </form>"
          (escape-html result)
          (get-translation "submit" language)))
```

3. **Use DaisyUI components**:
```lisp
(defun wrap-in-card (content &key title)
  "Wrap content in DaisyUI card"
  (format nil "<div class=\"card bg-base-200 shadow-xl\">
                 <div class=\"card-body\">
                   ~@[<h2 class=\"card-title\">~A</h2>~]
                   ~A
                 </div>
               </div>"
          title content))
```

### Session Management Best Practices

1. **Always get session ID from request**:
```lisp
(defun get-session-id (params)
  "Extract or generate session ID"
  (or (cdr (assoc "session-id" params :test #'string=))
      (generate-session-id)))
```

2. **Store conversation history**:
```lisp
(add-to-conversation-history session-id "character-chat" "user" message)
(add-to-conversation-history session-id "character-chat" "assistant" response)
```

3. **Retrieve history for display**:
```lisp
(let ((history (get-conversation-history session-id "character-chat")))
  (generate-chat-interface language history))
```

### Code Style

- Follow Common Lisp conventions
- Use descriptive function and variable names
- Add docstrings to all public functions
- Write tests for new functionality
- Keep functions focused and modular
- Always return HTML from handlers (not JSON)
- Include HTMX attributes in generated HTML
- Escape all user input before including in HTML

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

MIT

## Support

For issues, questions, or contributions, please [open an issue](link-to-issues) on the project repository.
