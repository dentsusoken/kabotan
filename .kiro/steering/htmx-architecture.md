# HTMX Architecture Guidelines

## Core Principles

This project follows HTMX's hypermedia-driven architecture principles. The server generates HTML responses, and the client uses declarative HTML attributes for interactions. This approach provides:

1. **Locality of Behaviour**: HTML attributes describe behaviour directly where it's used
2. **Server-Driven Content**: All HTML generation happens on the server
3. **Minimal Client-Side JavaScript**: Only essential functionality not provided by HTMX
4. **Simplified State Management**: Server-side session storage for application state

## Architecture Overview

### Request Flow

```
User Action (click/submit)
  ↓
HTMX sends HTTP request (GET/POST)
  ↓
Server generates HTML fragment
  ↓
HTMX swaps HTML into DOM
  ↓
User sees updated content
```

### Streaming Flow

```
User submits form
  ↓
HTMX establishes SSE connection (sse-connect)
  ↓
Server sends HTML fragments as SSE events
  ↓
HTMX swaps fragments into DOM (sse-swap)
  ↓
Server sends completion event
  ↓
HTMX closes connection automatically
```

## Server-Side Responsibilities

### HTML Generation

**All HTML must be generated on the server**, not in client-side JavaScript.

#### Required Practices

1. **Return HTML fragments from API endpoints**:
```lisp
(defun handle-feature-request (params)
  (let* ((session-id (get-session-id params))
         (language (get-session-language session-id))
         (html (generate-feature-html params language)))
    `(200 (:content-type "text/html; charset=utf-8")
      (,html))))
```

2. **Include HTMX attributes in generated HTML**:
```lisp
(defun generate-form-html (action-url language)
  (format nil "<form hx-post=\"~A\" 
                     hx-target=\"#result\" 
                     hx-swap=\"innerHTML\">
                 ~A
               </form>"
          action-url
          (generate-form-fields language)))
```

3. **Escape all user-generated content**:
```lisp
(defun format-user-message (content)
  (format nil "<div class=\"chat-bubble\">~A</div>"
          (escape-html content)))
```

#### Prohibited Practices

❌ **Never return JSON for UI rendering**:
```lisp
;; DON'T DO THIS
(defun handle-request (params)
  `(200 (:content-type "application/json")
    (,(jonathan:to-json (list :result result)))))
```

❌ **Never expect client-side HTML generation**:
```javascript
// DON'T DO THIS
function renderResult(data) {
  return `<div>${data.result}</div>`;
}
```

### Session Management

**All application state must be managed server-side** using session storage.

#### Required Practices

1. **Store language preferences in sessions**:
```lisp
(defun set-session-language (session-id language)
  (setf (gethash session-id *session-languages*) language))
```

2. **Store conversation history in sessions**:
```lisp
(defun add-to-conversation-history (session-id feature role content)
  (push (list :role role :content content :timestamp (get-universal-time))
        (gethash (cons session-id feature) *conversation-histories*)))
```

3. **Include session data in HTML responses**:
```lisp
(defun generate-chat-html (session-id character language)
  (let ((history (get-conversation-history session-id character)))
    (format nil "~{~A~}~A"
            (mapcar #'format-message history)
            (generate-input-form character language))))
```

#### Prohibited Practices

❌ **Never manage state in client-side JavaScript**:
```javascript
// DON'T DO THIS
let conversationHistory = [];
function addMessage(message) {
  conversationHistory.push(message);
}
```

### Streaming Responses

**Streaming must use SSE protocol with HTML fragments**, not JSON chunks.

#### Required Practices

1. **Send HTML fragments as SSE events**:
```lisp
(defun stream-response (writer content)
  (funcall writer 
    (format-sse-event "message" 
                      (escape-html content))))
```

2. **Include proper SSE event names**:
```lisp
;; Content updates
(format-sse-event "message" html-fragment)

;; Completion
(format-sse-event "complete" completion-html)

;; Errors
(format-sse-event "error" error-html)
```

3. **Generate complete HTML for each event**:
```lisp
(defun format-streaming-chunk (chunk)
  (format nil "<span class=\"chunk\">~A</span>" 
          (escape-html chunk)))
```

#### Prohibited Practices

❌ **Never send JSON in SSE events**:
```lisp
;; DON'T DO THIS
(funcall writer 
  (format-sse-data 
    (jonathan:to-json (list :chunk chunk :type "content"))))
```

❌ **Never expect client-side parsing of SSE data**:
```javascript
// DON'T DO THIS
eventSource.addEventListener('message', (e) => {
  const data = JSON.parse(e.data);
  renderChunk(data.chunk);
});
```

## Client-Side Responsibilities

### HTMX Attributes

**Use HTMX attributes for all dynamic interactions**.

#### Core Attributes

1. **Request attributes**:
   - `hx-get`: Load content via GET request
   - `hx-post`: Submit data via POST request
   - `hx-put`, `hx-patch`, `hx-delete`: Other HTTP methods

2. **Target attributes**:
   - `hx-target`: Specify where to swap content (CSS selector)
   - `hx-swap`: Control swap strategy (innerHTML, outerHTML, beforeend, etc.)

3. **Extension attributes**:
   - `hx-ext="sse"`: Enable SSE extension
   - `hx-ext="response-targets"`: Enable error handling extension
   - `sse-connect`: Establish SSE connection
   - `sse-swap`: Specify event name and swap target

4. **Additional attributes**:
   - `hx-vals`: Add dynamic values to requests
   - `hx-indicator`: Show loading indicator
   - `hx-trigger`: Customize trigger events
   - `hx-target-5*`, `hx-target-4*`: Error response targets

#### Examples

**Feature loading**:
```html
<button hx-get="/api/features/monster-diagnostic"
        hx-target="#feature-content"
        hx-swap="innerHTML">
  Monster Diagnostic
</button>
```

**Form submission**:
```html
<form hx-post="/api/monster-diagnostic"
      hx-target="#result-container"
      hx-swap="innerHTML"
      hx-indicator="#loading">
  <input type="text" name="favorite_food" required />
  <button type="submit">Submit</button>
  <span id="loading" class="htmx-indicator">Loading...</span>
</form>
```

**Streaming with SSE**:
```html
<div hx-ext="sse"
     sse-connect="/api/character-chat-stream?character=dracula&message=hello"
     sse-swap="message:#chat-container:beforeend">
  <div id="chat-container"></div>
</div>
```

**Error handling**:
```html
<form hx-post="/api/endpoint"
      hx-target="#result"
      hx-target-5*="#error-display"
      hx-target-4*="#error-display">
  <!-- Form fields -->
</form>
<div id="error-display"></div>
```

### Minimal JavaScript

**JavaScript should only implement functionality not provided by HTMX**.

#### Allowed JavaScript Use Cases

1. **Language preference management** (localStorage):
```javascript
function setLanguage(lang) {
  localStorage.setItem('language', lang);
  document.body.setAttribute('hx-vals', 
    JSON.stringify({language: lang}));
}
```

2. **Page initialization**:
```javascript
document.addEventListener('DOMContentLoaded', () => {
  const lang = localStorage.getItem('language') || 'ja';
  setLanguage(lang);
  htmx.ajax('GET', `/api/features/default?language=${lang}`, 
    {target: '#feature-content'});
});
```

3. **HTMX event listeners** (optional enhancements):
```javascript
document.body.addEventListener('htmx:afterSwap', (evt) => {
  // Optional: scroll to new content, focus input, etc.
});
```

#### Prohibited JavaScript Use Cases

❌ **HTML generation**:
```javascript
// DON'T DO THIS
function renderForm(data) {
  return `<form>...</form>`;
}
```

❌ **State management**:
```javascript
// DON'T DO THIS
let appState = {
  currentFeature: 'monster',
  history: []
};
```

❌ **Custom AJAX/fetch calls**:
```javascript
// DON'T DO THIS - Use HTMX attributes instead
fetch('/api/endpoint')
  .then(r => r.json())
  .then(data => updateUI(data));
```

❌ **Manual DOM manipulation**:
```javascript
// DON'T DO THIS - Let HTMX handle it
document.getElementById('result').innerHTML = html;
```

❌ **Custom EventSource for SSE**:
```javascript
// DON'T DO THIS - Use HTMX SSE extension
const eventSource = new EventSource('/api/stream');
eventSource.onmessage = (e) => {
  // Manual handling
};
```

## File Organization

### Frontend Files

**index.html** (Minimal shell):
- Page structure (header, nav, main, footer)
- HTMX library and extensions
- Minimal JavaScript modules
- Empty content containers

**JavaScript modules** (Minimal, focused):
- `language-manager.js`: Language preference only
- `page-init.js`: Page initialization only
- No HTML templates, no rendering logic, no state management

### Backend Files

**HTML generation** (`src/utils/html-*.lisp`):
- `html-common.lisp`: Base HTML utilities and DaisyUI components
- `html-forms.lisp`: Form components (depends on html-common)
- `html-features.lisp`: Feature-specific forms (depends on html-common and html-forms)
- `html-chat.lisp`: Chat/conversation components (depends on html-common and html-forms)

**Session management** (`src/services/session-manager.lisp`):
- Language preference storage
- Conversation history storage
- Session lifecycle management

**API handlers** (`src/api/handlers/*.lisp`):
- Return HTML fragments
- Use session data
- Include HTMX attributes in responses

## Testing Guidelines

### Backend Tests

Test HTML generation:
```lisp
(test generate-form-html
  (let ((html (generate-monster-diagnostic-form "en")))
    (is (search "hx-post=\"/api/monster-diagnostic\"" html))
    (is (search "required" html))
    (is (search "Favorite Food" html))))
```

Test session management:
```lisp
(test session-persistence
  (let ((session-id "test-123"))
    (set-session-language session-id "en")
    (is (string= "en" (get-session-language session-id)))))
```

### E2E Tests

Test HTMX interactions:
```javascript
test('should load feature via HTMX', async ({ page }) => {
  await page.goto('/');
  await page.click('[hx-get*="monster-diagnostic"]');
  await expect(page.locator('form[hx-post*="monster-diagnostic"]'))
    .toBeVisible();
});
```

Test streaming:
```javascript
test('should stream responses', async ({ page }) => {
  await page.goto('/');
  await page.click('[hx-get*="character-chat"]');
  await page.fill('[name="message"]', 'Hello');
  await page.click('button[type="submit"]');
  await expect(page.locator('[sse-swap]')).toBeVisible();
  await expect(page.locator('.chat-bubble')).toBeVisible();
});
```

## Migration Checklist

When adding new features or refactoring existing ones:

- [ ] Server returns HTML fragments, not JSON
- [ ] All HTML generation is in `src/utils/html-*.lisp` files
- [ ] HTMX attributes included in generated HTML
- [ ] User content is escaped with `escape-html`
- [ ] Session storage used for state management
- [ ] Streaming uses SSE with HTML fragments
- [ ] No client-side HTML generation
- [ ] No client-side state management
- [ ] No custom AJAX/fetch calls
- [ ] No manual DOM manipulation
- [ ] Backend tests verify HTML structure
- [ ] E2E tests verify HTMX interactions

## Common Pitfalls

### Pitfall 1: Returning JSON instead of HTML

❌ **Wrong**:
```lisp
(defun handle-request (params)
  `(200 (:content-type "application/json")
    (,(jonathan:to-json result))))
```

✅ **Correct**:
```lisp
(defun handle-request (params)
  (let ((html (generate-result-html result language)))
    `(200 (:content-type "text/html; charset=utf-8")
      (,html))))
```

### Pitfall 2: Client-side HTML generation

❌ **Wrong**:
```javascript
function renderResult(data) {
  return `<div class="result">${data.content}</div>`;
}
```

✅ **Correct**:
```lisp
(defun generate-result-html (content)
  (format nil "<div class=\"result\">~A</div>" 
          (escape-html content)))
```

### Pitfall 3: Custom EventSource for streaming

❌ **Wrong**:
```javascript
const eventSource = new EventSource('/api/stream');
eventSource.onmessage = (e) => {
  const data = JSON.parse(e.data);
  appendToDOM(data.chunk);
};
```

✅ **Correct**:
```html
<div hx-ext="sse"
     sse-connect="/api/stream"
     sse-swap="message:#result:beforeend">
  <div id="result"></div>
</div>
```

### Pitfall 4: Client-side state management

❌ **Wrong**:
```javascript
let conversationHistory = [];
function addMessage(msg) {
  conversationHistory.push(msg);
}
```

✅ **Correct**:
```lisp
(defun add-to-conversation-history (session-id feature role content)
  (push (list :role role :content content)
        (gethash (cons session-id feature) *conversation-histories*)))
```

## References

- [HTMX Documentation](https://htmx.org/docs/)
- [HTMX SSE Extension](https://htmx.org/extensions/server-sent-events/)
- [HTMX Response Targets Extension](https://htmx.org/extensions/response-targets/)
- [Hypermedia Systems Book](https://hypermedia.systems/)
- Spec: `.kiro/specs/frontend-22-htmx-refactoring/`
