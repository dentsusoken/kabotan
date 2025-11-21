# Design Document

## Overview

This design document outlines the architectural refactoring of the Kabotan frontend from a JavaScript-heavy, client-side rendering approach to an HTMX-driven, server-side rendering architecture. The refactoring aligns with HTMX's hypermedia-driven philosophy where the server generates HTML responses and the client uses declarative HTML attributes for interactions.

### Current Architecture Problems

1. **Client-side HTML generation**: JavaScript files contain HTML templates and rendering logic
2. **Custom SSE implementation**: Manual EventSource management duplicates HTMX SSE extension functionality
3. **Client-side state management**: Conversation history and UI state managed in JavaScript
4. **JSON API responses**: Server returns JSON instead of HTML, requiring client-side transformation
5. **Scattered logic**: Feature logic split between server (business logic) and client (rendering)

### Target Architecture Benefits

1. **Server-driven HTML**: All HTML generation happens on the server
2. **Declarative interactions**: HTMX attributes replace imperative JavaScript
3. **Simplified client**: Minimal JavaScript for essential functionality only
4. **Locality of Behaviour**: HTML attributes describe behaviour directly
5. **Better maintainability**: Single source of truth for rendering logic

## Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  index.html (Shell)                                     │ │
│  │  - Header with language toggle                          │ │
│  │  - Feature tabs with hx-get attributes                  │ │
│  │  - Empty content container                              │ │
│  │  - HTMX + SSE Extension loaded                          │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Minimal JavaScript                                     │ │
│  │  - Language preference management                       │ │
│  │  - HTMX event listeners (optional enhancements)         │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ HTMX Requests
                            │ (HTML responses)
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Common Lisp Server                        │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  HTML Generation Layer (NEW)                           │ │
│  │  - Feature form generators                             │ │
│  │  - Response formatters (HTML fragments)                │ │
│  │  - Template rendering utilities                        │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Session Management (NEW)                              │ │
│  │  - Language preference storage                         │ │
│  │  - Conversation history storage                        │ │
│  │  - Session-based state management                      │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  API Handlers (MODIFIED)                               │ │
│  │  - Return HTML instead of JSON                         │ │
│  │  - SSE responses send HTML fragments                   │ │
│  │  - Include HTMX attributes in responses                │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Business Logic Layer (UNCHANGED)                      │ │
│  │  - LLM service integration                             │ │
│  │  - Prompt building                                     │ │
│  │  - Validation                                          │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Request Flow Comparison

#### Current Flow (JSON-based)
```
User clicks tab
  → JavaScript switches content
  → JavaScript renders form from template
  → User submits form
  → HTMX sends request
  → Server returns JSON
  → JavaScript parses JSON
  → JavaScript generates HTML
  → JavaScript updates DOM
```

#### New Flow (HTML-based)
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

### Streaming Flow Comparison

#### Current Flow (Custom EventSource)
```
User submits form
  → JavaScript creates EventSource
  → JavaScript manages connection
  → Server sends SSE with JSON chunks
  → JavaScript parses JSON
  → JavaScript appends to DOM
  → JavaScript handles completion
  → JavaScript closes connection
```

#### New Flow (HTMX SSE Extension)
```
User submits form
  → HTMX establishes SSE connection
  → Server sends SSE with HTML fragments
  → HTMX swaps HTML fragments
  → Server sends completion event
  → HTMX closes connection automatically
```

## Components and Interfaces

### Frontend Components

#### 1. index.html (Shell)

**Purpose**: Minimal HTML shell that loads dynamically

**Structure**:
```html
<!DOCTYPE html>
<html lang="ja">
<head>
  <!-- TailwindCSS, DaisyUI, HTMX core -->
  <script src="htmx.min.js"></script>
  <script src="htmx-ext-sse.js"></script>
  <script src="htmx-ext-response-targets.js"></script>
  <script src="language-manager.js"></script>
</head>
<body hx-ext="sse,response-targets">
  <header>
    <h1>🎃 Kabotan</h1>
    <button id="language-toggle" 
            hx-post="/api/set-language"
            hx-vals='{"language": "en"}'
            hx-target="#feature-content"
            hx-swap="outerHTML">
      Language
    </button>
  </header>
  
  <nav>
    <button hx-get="/api/features/monster-diagnostic"
            hx-target="#feature-content"
            hx-swap="innerHTML">
      Monster Diagnostic
    </button>
    <!-- Other feature tabs -->
  </nav>
  
  <main id="feature-content">
    <!-- Feature content loaded here -->
  </main>
  
  <div id="error-display"></div>
</body>
</html>
```

**HTMX Attributes**:
- `hx-ext="sse,response-targets"`: Enable extensions globally
- `hx-get`: Load feature content
- `hx-post`: Submit forms and change language
- `hx-target`: Specify where to swap content
- `hx-swap`: Control swap strategy
- `hx-vals`: Add dynamic values to requests

#### 2. language-manager.js (Minimal)

**Purpose**: Manage language preference in localStorage and update HTMX requests

**Responsibilities**:
- Store language preference in localStorage
- Update hx-vals on language toggle
- Trigger content reload on language change

**Size**: ~50 lines (reduced from ~200 lines)

```javascript
// Store language preference
function setLanguage(lang) {
  localStorage.setItem('language', lang);
  // Update all HTMX requests to include language
  document.body.setAttribute('hx-vals', 
    JSON.stringify({language: lang}));
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
  const lang = localStorage.getItem('language') || 'ja';
  setLanguage(lang);
  
  // Load default feature
  htmx.ajax('GET', `/api/features/monster-diagnostic?language=${lang}`, 
    {target: '#feature-content'});
});
```

### Backend Components

#### 1. HTML Template Generator (NEW)

**File**: `src/utils/html-templates.lisp`

**Purpose**: Generate HTML fragments for features and responses

**Key Functions**:

```lisp
(defun generate-feature-form (feature-name language)
  "Generate HTML form for a feature with localized labels")

(defun generate-monster-diagnostic-form (language)
  "Generate monster diagnostic form with inputs and HTMX attributes")

(defun generate-character-chat-form (language &optional history)
  "Generate chat interface with message history and input form")

(defun generate-streaming-container (feature-name)
  "Generate container with SSE attributes for streaming responses")

(defun wrap-in-card (content &key title class)
  "Wrap content in DaisyUI card component")
```

**Example Output**:
```html
<div class="card bg-base-200 shadow-xl">
  <div class="card-body">
    <h2 class="card-title">Monster Diagnostic</h2>
    <form hx-post="/api/monster-diagnostic"
          hx-target="#result-container"
          hx-swap="innerHTML"
          hx-indicator="#loading">
      <label>
        <span>Favorite Food</span>
        <input type="text" name="favorite_food" required />
      </label>
      <!-- More inputs -->
      <button type="submit">Diagnose</button>
      <span id="loading" class="htmx-indicator">Loading...</span>
    </form>
    <div id="result-container"></div>
  </div>
</div>
```

#### 2. Session Manager (NEW)

**File**: `src/services/session-manager.lisp`

**Purpose**: Manage server-side session state

**Key Functions**:

```lisp
(defun get-session-language (session-id)
  "Retrieve language preference from session")

(defun set-session-language (session-id language)
  "Store language preference in session")

(defun get-conversation-history (session-id feature)
  "Retrieve conversation history for a feature")

(defun add-to-conversation-history (session-id feature role content)
  "Add message to conversation history")

(defun clear-conversation-history (session-id feature)
  "Clear conversation history for a feature")
```

**Implementation**:
- Use hash table for in-memory session storage
- Session ID from cookie or generated on first request
- Automatic cleanup of old sessions

#### 3. Modified API Handlers

**Changes to Existing Handlers**:

1. **Return HTML instead of JSON**:
```lisp
;; Before
(defun handle-monster-diagnostic (params)
  `(200 (:content-type "application/json")
    (,(jonathan:to-json (list :result result)))))

;; After
(defun handle-monster-diagnostic (params)
  (let* ((session-id (get-session-id params))
         (language (get-session-language session-id))
         (result (process-monster-diagnostic params))
         (html (format-monster-diagnostic-result result language)))
    `(200 (:content-type "text/html; charset=utf-8")
      (,html))))
```

2. **SSE responses send HTML fragments**:
```lisp
;; Before
(funcall writer (format-sse-data 
                 (list (cons :chunk chunk)
                       (cons :type "content"))))

;; After
(funcall writer (format-sse-event 
                 "message"
                 (escape-html chunk)))
```

3. **Include HTMX attributes in responses**:
```lisp
(defun format-chat-message-with-form (message character language)
  "Return chat message HTML with form for next message"
  (format nil "~A
<form hx-post=\"/api/character-chat\"
      hx-target=\"#chat-container\"
      hx-swap=\"beforeend\"
      hx-vals='{\"character\": \"~A\"}'>
  <input type=\"text\" name=\"message\" required />
  <button type=\"submit\">Send</button>
</form>"
    (format-chat-message message character)
    character))
```

#### 4. Feature Content Endpoints (NEW)

**Purpose**: Serve initial feature forms

**Endpoints**:
- `GET /api/features/monster-diagnostic`
- `GET /api/features/story-generator`
- `GET /api/features/character-chat`
- `GET /api/features/trivia-bot`
- `GET /api/features/spell-generator`

**Response**: Complete HTML form with HTMX attributes

**Example Handler**:
```lisp
(defun handle-get-feature (feature-name params)
  "Return HTML form for the specified feature"
  (let* ((session-id (get-session-id params))
         (language (get-session-language session-id))
         (html (generate-feature-form feature-name language)))
    `(200 (:content-type "text/html; charset=utf-8")
      (,html))))
```

## Data Models

### Session Data Structure

```lisp
(defstruct session
  (id nil :type string)
  (language "ja" :type string)
  (created-at (get-universal-time) :type integer)
  (last-accessed (get-universal-time) :type integer)
  (conversations (make-hash-table :test 'equal) :type hash-table))

;; Conversation history structure
;; Key: feature name (string)
;; Value: list of messages
;; Message: (:role "user" :content "..." :timestamp 123456)
```

### HTML Template Parameters

```lisp
;; Feature form parameters
(defstruct form-params
  feature-name
  language
  action-url
  method
  target-id
  swap-strategy
  fields
  submit-label
  streaming-p)

;; Field definition
(defstruct field
  name
  type
  label
  required-p
  options
  placeholder
  value)
```

## Error Handling

### Error Response Strategy

1. **Validation Errors**: Return HTML with error messages inline
```html
<div class="alert alert-error">
  <p>Please check your input:</p>
  <ul>
    <li>Favorite food is required</li>
  </ul>
</div>
```

2. **Server Errors**: Use HTMX response-targets extension
```html
<!-- In form -->
<form hx-post="/api/endpoint"
      hx-target="#result"
      hx-target-5*="#error-display">
  ...
</form>

<!-- Error response (500) -->
<div class="alert alert-error">
  <p>Server error occurred. Please try again.</p>
  <button hx-post="/api/endpoint" hx-target="#result">
    Retry
  </button>
</div>
```

3. **Streaming Errors**: Send error event via SSE
```lisp
(funcall writer (format-sse-event 
                 "error"
                 "<div class=\"alert alert-error\">
                    Connection error
                  </div>"))
```

### HTMX Error Events

Listen to HTMX events for error handling:
```javascript
document.body.addEventListener('htmx:responseError', (evt) => {
  console.error('Response error:', evt.detail);
});

document.body.addEventListener('htmx:sendError', (evt) => {
  console.error('Network error:', evt.detail);
});
```

## Testing Strategy

### Backend Testing

1. **HTML Generation Tests**:
```lisp
(test generate-monster-diagnostic-form
  (let ((html (generate-monster-diagnostic-form "en")))
    (is (search "Favorite Food" html))
    (is (search "hx-post=\"/api/monster-diagnostic\"" html))
    (is (search "required" html))))
```

2. **Session Management Tests**:
```lisp
(test session-language-persistence
  (let ((session-id "test-123"))
    (set-session-language session-id "en")
    (is (string= "en" (get-session-language session-id)))))
```

3. **Response Format Tests**:
```lisp
(test html-response-format
  (let ((response (handle-monster-diagnostic test-params)))
    (is (= 200 (first response)))
    (is (search "text/html" (getf (second response) :content-type)))
    (is (search "<div" (third response)))))
```

### Frontend Testing (E2E)

1. **Feature Loading Tests**:
```javascript
test('should load monster diagnostic form', async ({ page }) => {
  await page.goto('/');
  await page.click('[hx-get*="monster-diagnostic"]');
  await expect(page.locator('form[hx-post*="monster-diagnostic"]'))
    .toBeVisible();
});
```

2. **Form Submission Tests**:
```javascript
test('should submit form and display result', async ({ page }) => {
  await page.goto('/');
  await page.click('[hx-get*="monster-diagnostic"]');
  await page.fill('[name="favorite_food"]', 'Pizza');
  // Fill other fields
  await page.click('button[type="submit"]');
  await expect(page.locator('#result-container .alert-success'))
    .toBeVisible();
});
```

3. **Streaming Tests**:
```javascript
test('should stream chat responses', async ({ page }) => {
  await page.goto('/');
  await page.click('[hx-get*="character-chat"]');
  await page.fill('[name="message"]', 'Hello');
  await page.click('button[type="submit"]');
  
  // Wait for streaming to start
  await expect(page.locator('[sse-swap]')).toBeVisible();
  
  // Wait for completion
  await expect(page.locator('.chat-bubble')).toBeVisible();
});
```

### Integration Testing

Test HTMX SSE extension integration:
```javascript
test('should handle SSE connection', async ({ page }) => {
  await page.goto('/');
  
  // Monitor SSE events
  const sseEvents = [];
  page.on('console', msg => {
    if (msg.text().includes('SSE')) {
      sseEvents.push(msg.text());
    }
  });
  
  // Trigger streaming request
  await page.click('[hx-get*="character-chat"]');
  await page.fill('[name="message"]', 'Test');
  await page.click('button[type="submit"]');
  
  // Verify SSE events
  await page.waitForTimeout(2000);
  expect(sseEvents.length).toBeGreaterThan(0);
});
```

## Migration Strategy

### Phase 1: Backend HTML Generation

1. Create `html-templates.lisp` with form generators
2. Create `session-manager.lisp` for session handling
3. Add feature content endpoints (`GET /api/features/*`)
4. Test HTML generation in isolation

### Phase 2: Convert One Feature

1. Choose simplest feature (spell-generator)
2. Modify handler to return HTML
3. Update index.html to load feature via HTMX
4. Test end-to-end functionality
5. Verify E2E tests pass

### Phase 3: Convert Remaining Features

1. Convert monster-diagnostic
2. Convert story-generator
3. Convert character-chat (with history)
4. Convert trivia-bot (with history)
5. Update all E2E tests

### Phase 4: Implement Streaming

1. Add HTMX SSE extension to index.html
2. Modify streaming handlers to send HTML fragments
3. Update forms to use sse-connect and sse-swap
4. Test streaming functionality
5. Update streaming E2E tests

### Phase 5: Cleanup

1. Remove unused JavaScript files:
   - feature-content-templates.js
   - feature-ui-handlers.js
   - feature-streaming-handlers.js
   - streaming-manager.js
   - chat-manager.js
   - feature-manager.js (most of it)
2. Simplify remaining JavaScript
3. Update documentation
4. Remove old API endpoints if any

### Phase 6: Language Switching

1. Implement session-based language storage
2. Add language toggle endpoint
3. Update all forms to include language parameter
4. Test language switching across features

## Performance Considerations

### Server-Side Rendering Performance

1. **Template Caching**: Cache generated HTML templates
```lisp
(defparameter *template-cache* (make-hash-table :test 'equal))

(defun get-cached-template (key generator-fn)
  (or (gethash key *template-cache*)
      (setf (gethash key *template-cache*)
            (funcall generator-fn))))
```

2. **Session Cleanup**: Periodic cleanup of old sessions
```lisp
(defun cleanup-old-sessions ()
  "Remove sessions older than 24 hours"
  (let ((cutoff (- (get-universal-time) (* 24 60 60))))
    (maphash (lambda (id session)
               (when (< (session-last-accessed session) cutoff)
                 (remhash id *sessions*)))
             *sessions*)))
```

3. **HTML Minification**: Remove unnecessary whitespace
```lisp
(defun minify-html (html)
  "Remove extra whitespace from HTML"
  (cl-ppcre:regex-replace-all "\\s+" html " "))
```

### Client-Side Performance

1. **Reduced JavaScript**: Smaller bundle size
2. **Browser Caching**: Static assets cached effectively
3. **HTMX Efficiency**: Minimal DOM manipulation overhead
4. **SSE Connection Reuse**: HTMX manages connections efficiently

## Security Considerations

### Input Validation

All input validation remains on server-side:
```lisp
(defun validate-and-sanitize-input (input)
  "Validate and sanitize user input"
  (when (> (length input) 1000)
    (error "Input too long"))
  (escape-html input))
```

### Session Security

1. **Secure Session IDs**: Use cryptographically secure random IDs
2. **Session Expiration**: Automatic cleanup of old sessions
3. **CSRF Protection**: Include CSRF tokens in forms
```html
<form hx-post="/api/endpoint">
  <input type="hidden" name="csrf-token" value="{{csrf-token}}" />
  ...
</form>
```

### XSS Prevention

1. **HTML Escaping**: All user content escaped
2. **Content-Type Headers**: Proper content-type for HTML responses
3. **CSP Headers**: Content Security Policy headers

```lisp
(defun add-security-headers (response)
  "Add security headers to response"
  (let ((headers (second response)))
    (setf (getf headers :content-security-policy)
          "default-src 'self'; script-src 'self' cdn.jsdelivr.net")
    response))
```

## Deployment Considerations

### Docker Configuration

No changes required to Dockerfile - same deployment process

### Environment Variables

No new environment variables required

### Monitoring

Add logging for:
1. Session creation/cleanup
2. HTML generation performance
3. SSE connection lifecycle
4. Error rates by feature

```lisp
(defun log-session-event (event session-id)
  "Log session-related events"
  (format t "[SESSION] ~A: ~A~%" event session-id))

(defun log-html-generation (feature duration)
  "Log HTML generation performance"
  (format t "[HTML-GEN] ~A: ~Ams~%" feature duration))
```

## Rollback Plan

If issues arise during migration:

1. **Feature-by-Feature Rollback**: Keep old JavaScript files until all features converted
2. **Feature Flags**: Use server-side flags to enable/disable new implementation
```lisp
(defparameter *use-html-responses* t)

(defun handle-request (params)
  (if *use-html-responses*
      (handle-request-html params)
      (handle-request-json params)))
```
3. **Gradual Migration**: Deploy one feature at a time
4. **Monitoring**: Watch error rates and performance metrics

## Future Enhancements

### Potential Improvements

1. **Template Engine**: Consider using a Lisp HTML template engine (e.g., CL-WHO, Djula)
2. **Component Library**: Build reusable HTML component functions
3. **Progressive Enhancement**: Add optional JavaScript enhancements
4. **WebSocket Support**: Consider HTMX WebSocket extension for real-time features
5. **Offline Support**: Service worker for offline functionality
6. **Server-Side Validation Messages**: More detailed inline validation

### HTMX Advanced Features

1. **Out-of-Band Swaps**: Update multiple page areas simultaneously
2. **History Support**: Browser back/forward navigation
3. **Polling**: Auto-refresh content at intervals
4. **Lazy Loading**: Load content on scroll
5. **Optimistic UI**: Show updates before server confirmation
