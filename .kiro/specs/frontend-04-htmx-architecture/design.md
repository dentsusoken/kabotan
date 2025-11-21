# HTMX Frontend Integration - Design

## Architecture Overview

The frontend architecture follows a modular JavaScript approach with HTMX for server communication. The design emphasizes separation of concerns, progressive enhancement, and graceful degradation.

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
├─────────────────────────────────────────────────────────────┤
│  HTML (index.html)                                          │
│  ├─ HTMX Attributes (hx-post, hx-target, hx-swap)          │
│  ├─ DaisyUI Components (tabs, cards, forms)                │
│  └─ Custom Styles (Halloween theme)                        │
├─────────────────────────────────────────────────────────────┤
│  JavaScript Modules                                         │
│  ├─ feature-manager.js    (Feature switching, streaming)   │
│  ├─ language-manager.js   (i18n, translations)             │
│  ├─ streaming-manager.js  (SSE connection management)      │
│  ├─ chat-manager.js       (Conversation history)           │
│  └─ error-handler.js      (Error display)                  │
├─────────────────────────────────────────────────────────────┤
│  State Management                                           │
│  ├─ sessionStorage        (Conversation history)           │
│  ├─ localStorage          (Language preference)            │
│  └─ window object         (Active streaming managers)      │
└─────────────────────────────────────────────────────────────┘
                            ↕ HTTP/SSE
┌─────────────────────────────────────────────────────────────┐
│                    Backend API (Ningle)                     │
│  ├─ /api/monster-diagnostic      (POST)                    │
│  ├─ /api/monster-diagnostic-stream (GET SSE)               │
│  ├─ /api/story-generator          (POST)                   │
│  ├─ /api/story-generator-stream   (GET SSE)                │
│  ├─ /api/character-chat           (POST)                   │
│  ├─ /api/character-chat-stream    (GET SSE)                │
│  ├─ /api/trivia-bot               (POST)                   │
│  ├─ /api/trivia-bot-stream        (GET SSE)                │
│  ├─ /api/spell-generator          (POST)                   │
│  └─ /api/spell-generator-stream   (GET SSE)                │
└─────────────────────────────────────────────────────────────┘
```

## Component Design

### 1. HTML Structure (index.html)

#### Layout Hierarchy
```html
<body>
  <header>
    <navbar>
      <logo>
      <language-toggle>
  </header>
  
  <main>
    <streaming-indicator>  <!-- Global streaming UI -->
    <tabs>                 <!-- Feature navigation -->
    <feature-content>      <!-- Dynamic content area -->
  </main>
  
  <footer>
</body>
```

#### HTMX Attributes Pattern
```html
<form hx-post="/api/endpoint"
      hx-target="#result-div"
      hx-swap="innerHTML"
      hx-indicator="#loading-spinner">
  <!-- Form fields -->
</form>
```

### 2. JavaScript Modules

#### feature-manager.js
**Responsibilities:**
- Feature tab switching
- Dynamic content generation
- Streaming initialization
- Form submission handling
- Error display coordination

**Key Functions:**
```javascript
switchFeature(featureName)           // Switch active feature tab
startMonsterDiagnosticStreaming()    // Start streaming for monster diagnostic
startStoryGeneratorStreaming()       // Start streaming for story generator
startSpellGeneratorStreaming()       // Start streaming for spell generator
stopActiveStreaming()                // Stop any active streaming
showStreamingError()                 // Display streaming error with retry/fallback
fallbackToNonStreaming()             // Fall back to HTMX mode
```

**Feature Content Templates:**
- Stored as string templates in `featureContent` object
- Include HTMX attributes for form submission
- Include error display areas
- Include result display areas
- Include language-specific placeholders

#### language-manager.js
**Responsibilities:**
- Language detection and initialization
- UI text translation
- Language toggle handling
- localStorage persistence

**Key Functions:**
```javascript
initLanguage()                       // Detect and set initial language
updateUIText()                       // Update all translatable elements
getText(key, lang)                   // Get translated text for key
setupLanguageToggle()                // Setup toggle event listener
```

**Translation Structure:**
```javascript
const uiTexts = {
  ja: { 'key': '日本語テキスト' },
  en: { 'key': 'English text' }
}
```

#### streaming-manager.js
**Responsibilities:**
- EventSource connection management
- Efficient buffer management
- Throttled DOM updates
- Error handling and retry logic
- Resource cleanup

**Key Features:**
- String builder pattern for efficient concatenation
- Throttled updates (10 per second max)
- requestAnimationFrame for smooth rendering
- Automatic retry with exponential backoff
- Graceful fallback to non-streaming

**Class Structure:**
```javascript
class StreamingManager {
  constructor(targetElement, options)
  start(url, params)                 // Start streaming connection
  stop(flush)                        // Stop and cleanup
  destroy()                          // Complete resource release
  retry()                            // Retry failed connection
  getStats()                         // Get performance metrics
  
  static isEventSourceSupported()    // Check browser support
  static getBrowserCompatibility()   // Get compatibility info
}
```

**Performance Optimizations:**
- Array-based string builder (O(1) append vs O(n) concatenation)
- Throttled DOM updates (100ms interval)
- requestAnimationFrame for smooth updates
- Single join operation per flush

#### chat-manager.js
**Responsibilities:**
- Conversation history management
- Chat form submission handling
- Streaming for chat features
- History persistence in sessionStorage

**Key Functions:**
```javascript
handleChatSubmit(event)              // Handle character chat submission
handleTriviaSubmit(event)            // Handle trivia bot submission
startCharacterChatStreaming()        // Start streaming for character chat
startTriviaBotStreaming()            // Start streaming for trivia bot
clearChat()                          // Clear chat history
clearTrivia()                        // Clear trivia history
addToHistory(history, role, content) // Add message to history
```

**History Management:**
- Maximum 10 messages per conversation
- Stored in sessionStorage
- Cleared when switching features
- Sent with each API request

#### error-handler.js
**Responsibilities:**
- HTMX error event handling
- Error message display
- Auto-hide functionality

**Key Functions:**
```javascript
displayError(target, errorType)     // Show error message
hideError(target)                    // Hide error message
getErrorMessage(errorType)           // Get localized error message
setupErrorHandlers()                 // Setup HTMX event listeners
```

**Error Types:**
- `response-error` - HTTP errors
- `timeout` - Request timeouts
- `chat-error` - Chat-specific errors
- `spell-error` - Spell generator errors

### 3. State Management

#### sessionStorage
**Purpose:** Temporary conversation history

**Keys:**
- `chat-history` - Character chat conversation
- `trivia-history` - Trivia bot conversation
- `streaming-compat-message-shown` - Browser compatibility message flag

**Structure:**
```javascript
[
  { role: 'user', content: 'message' },
  { role: 'assistant', content: 'response' }
]
```

#### localStorage
**Purpose:** Persistent user preferences

**Keys:**
- `language` - User's language preference ('ja' or 'en')

#### window object
**Purpose:** Active streaming manager references

**Properties:**
- `window.activeChatStreamingManager` - Character chat streaming
- `window.activeTriviaStreamingManager` - Trivia bot streaming
- `window.streamingErrorCallbacks` - Error retry/fallback callbacks

### 4. Streaming Architecture

#### Connection Flow
```
1. User submits form
2. Check if streaming is supported
3. If supported:
   a. Create StreamingManager instance
   b. Show streaming indicator
   c. Start SSE connection
   d. Receive and display chunks
   e. Handle completion or error
4. If not supported or error:
   a. Fall back to HTMX POST request
   b. Display full response at once
```

#### SSE Message Format
```javascript
// Content chunk
{ "chunk": "text content" }

// Completion
{ "type": "done", "done": true }

// Error
{ "type": "error", "error": "error message" }
```

#### Buffer Management
```javascript
// String builder pattern
bufferChunks = []                    // Array of string chunks
bufferChunks.push(chunk)             // O(1) append
content = bufferChunks.join('')      // Single join on flush
```

#### Throttling Strategy
```
Chunks received: ████████████████████ (20 chunks/sec)
DOM updates:     ████                 (10 updates/sec)
Efficiency:      50% reduction in DOM operations
```

### 5. Form Submission Flow

#### Standard HTMX Flow
```
1. User fills form
2. User clicks submit
3. HTMX intercepts submit
4. HTMX sends POST request
5. Server processes and returns HTML
6. HTMX swaps response into target
7. Loading indicator hides
```

#### Streaming-Enhanced Flow
```
1. User fills form
2. User clicks submit
3. JavaScript intercepts submit
4. Check streaming support
5a. If supported:
    - Create streaming manager
    - Start SSE connection
    - Display chunks progressively
5b. If not supported:
    - Fall back to HTMX flow
6. Handle completion or error
```

### 6. Error Handling Strategy

#### Error Hierarchy
```
1. Network Errors
   ├─ Connection timeout (10s)
   ├─ Connection interrupted
   └─ Connection refused

2. HTTP Errors
   ├─ 4xx Client errors
   └─ 5xx Server errors

3. Streaming Errors
   ├─ EventSource not supported
   ├─ Stream interrupted
   └─ Parse error

4. Application Errors
   ├─ Invalid form data
   └─ LLM service error
```

#### Error Recovery
```
Error Detected
    ↓
Preserve Partial Content (if any)
    ↓
Display Error Message
    ↓
Offer Options:
    ├─ Retry (same method)
    └─ Fallback (non-streaming)
```

### 7. Mobile Responsive Design

#### Breakpoints
- Mobile: < 768px
- Tablet: 768px - 1024px
- Desktop: > 1024px

#### Mobile Optimizations
```css
/* Horizontal scrolling tabs */
.tabs-boxed {
  overflow-x: auto;
  flex-wrap: nowrap;
}

/* Larger touch targets */
.btn { min-height: 3rem; }
.input { min-height: 3rem; }

/* Prevent iOS zoom */
input, textarea { font-size: 16px; }

/* Adjusted chat height */
#chat-messages { height: 50vh; }
```

## Data Flow Diagrams

### Feature Switching Flow
```
User clicks tab
    ↓
switchFeature(name)
    ↓
Stop active streaming
    ↓
Update tab UI
    ↓
Generate feature HTML
    ↓
Update content area
    ↓
Process HTMX attributes
    ↓
Update translations
    ↓
Initialize handlers
```

### Streaming Response Flow
```
Form submit
    ↓
Create StreamingManager
    ↓
Show indicator
    ↓
Start EventSource
    ↓
┌─────────────────┐
│ Receive chunk   │
│      ↓          │
│ Add to buffer   │
│      ↓          │
│ Throttle (100ms)│
│      ↓          │
│ Flush to DOM    │
└─────────────────┘
    ↓
Completion/Error
    ↓
Hide indicator
    ↓
Update history
    ↓
Cleanup resources
```

### Language Switching Flow
```
User toggles language
    ↓
Update currentLang
    ↓
Save to localStorage
    ↓
Update HTML lang attribute
    ↓
Update all [data-i18n] elements
    ↓
Update all [data-i18n-placeholder] elements
    ↓
Update hidden language fields
    ↓
Reload dynamic content (spell generator)
```

## Security Considerations

### XSS Prevention
- All user input escaped via `escapeHtml()` function
- No `innerHTML` with user content
- Use `textContent` for dynamic text

### CSRF Protection
- All POST requests include form data
- No sensitive operations via GET
- Session-based authentication (if implemented)

### Content Security Policy
```html
<meta http-equiv="Content-Security-Policy" 
      content="default-src 'self'; 
               script-src 'self' https://cdn.tailwindcss.com https://unpkg.com; 
               style-src 'self' 'unsafe-inline' https://cdn.jsdelivr.net;">
```

## Performance Metrics

### Target Metrics
- Time to Interactive: < 2s
- First Contentful Paint: < 1s
- Streaming latency: < 100ms
- DOM update frequency: 10 Hz (max)
- Memory usage: < 50MB for long conversations

### Monitoring Points
- StreamingManager.getStats() for throttling efficiency
- Console logs for connection status
- Browser DevTools for network timing
- Memory profiler for leak detection

## Testing Strategy

### Unit Testing
- Language manager translation logic
- Error message generation
- History management functions
- HTML escaping utility

### Integration Testing
- HTMX form submission
- Streaming connection lifecycle
- Error handling and recovery
- State persistence

### E2E Testing
- Feature switching
- Form submission and response
- Streaming display
- Language switching
- Mobile responsive behavior

## Browser Compatibility Matrix

| Feature | Chrome | Firefox | Safari | Edge | Fallback |
|---------|--------|---------|--------|------|----------|
| HTMX | ✓ | ✓ | ✓ | ✓ | N/A |
| EventSource | ✓ | ✓ | ✓ | ✓ | HTMX POST |
| localStorage | ✓ | ✓ | ✓ | ✓ | Session only |
| sessionStorage | ✓ | ✓ | ✓ | ✓ | Memory only |
| requestAnimationFrame | ✓ | ✓ | ✓ | ✓ | setTimeout |

## Future Enhancements

### Potential Improvements
1. Service Worker for offline support
2. IndexedDB for larger history storage
3. WebSocket for bidirectional communication
4. Virtual scrolling for long conversations
5. Progressive Web App (PWA) features
6. Client-side caching of responses
7. Optimistic UI updates
8. Undo/redo functionality

### Not Planned
- Complex state management libraries
- Full SPA routing
- Client-side rendering frameworks
- GraphQL integration
