# Design Document

## Overview

This document describes the design for adding streaming communication support to the Kabotan web application's LLM integration. The current implementation uses synchronous HTTP requests that wait for complete responses before displaying content to users. This enhancement will implement Server-Sent Events (SSE) streaming to deliver LLM responses incrementally, providing a more responsive user experience.

The design follows the existing architecture patterns in Kabotan, leveraging Common Lisp's capabilities for concurrent I/O and HTMX's built-in SSE support on the frontend. The implementation will be backward-compatible, allowing graceful fallback to non-streaming mode when needed.

## Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  HTMX SSE Extension                                    │ │
│  │  - Establishes EventSource connection                  │ │
│  │  - Receives stream chunks                              │ │
│  │  - Updates DOM incrementally                           │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ SSE Connection
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Kabotan Backend                           │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  API Layer (halloween-api.lisp)                        │ │
│  │  - Streaming endpoint handlers                         │ │
│  │  - SSE response formatting                             │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Service Layer (llm-service.lisp)                      │ │
│  │  - Streaming LLM client                                │ │
│  │  - Chunk parsing and buffering                         │ │
│  │  - Error handling and retry logic                      │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ HTTP Streaming
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              OpenAI-Compatible LLM Service                   │
│  - Accepts stream=true parameter                            │
│  - Returns SSE-formatted chunks                             │
│  - Format: data: {"choices":[{"delta":{"content":"..."}}]}  │
└─────────────────────────────────────────────────────────────┘
```

### Technology Stack

#### Backend
- **Common Lisp (SBCL)**: Core runtime
- **Ningle**: Web framework for routing
- **Clack/Lack**: HTTP server and middleware
- **Dexador**: HTTP client (with `:want-stream t` for streaming)
- **flexi-streams**: Stream encoding/decoding
- **cl-json**: JSON parsing for stream chunks
- **bordeaux-threads**: Thread management for async streaming

#### Frontend
- **HTMX**: Core framework with SSE extension
- **EventSource API**: Browser-native SSE support
- **JavaScript**: Custom streaming UI logic

#### LLM API
- **OpenAI Chat Completions API**: Streaming mode with `stream: true`
- **SSE Format**: Server-Sent Events protocol

### Design Principles

1. **Backward Compatibility**: Non-streaming mode remains available as fallback
2. **Progressive Enhancement**: Streaming enhances UX without breaking existing functionality
3. **Modularity**: Streaming logic is isolated in dedicated functions
4. **Error Resilience**: Graceful degradation when streaming fails
5. **Performance**: Efficient chunk processing and DOM updates
6. **Testability**: Mockable streaming interfaces for unit tests

## Components and Interfaces

### Backend Components

#### 1. Streaming LLM Service (`src/services/llm-service.lisp`)

**New Functions:**

```lisp
(defun call-llm-streaming (prompt callback &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the LLM service with streaming enabled, invoking callback for each chunk.
   
   Parameters:
   - prompt: The text prompt to send to the LLM
   - callback: Function called with each content chunk (string)
   - max-tokens: Maximum number of tokens in the response
   - temperature: Sampling temperature for response generation
   - timeout: Maximum time in seconds to wait for response
   
   Returns:
   - :success on successful completion
   - :error on failure"
  ...)

(defun call-llm-with-messages-streaming (messages callback &key (max-tokens 10000) (temperature 0.7) (timeout 360) system-prompt)
  "Calls the LLM service with message array and streaming enabled.
   
   Parameters:
   - messages: List of message plists with :role and :content keys
   - callback: Function called with each content chunk (string)
   - max-tokens: Maximum number of tokens in the response
   - temperature: Sampling temperature for response generation
   - timeout: Maximum time in seconds to wait for response
   - system-prompt: Optional system message to prepend
   
   Returns:
   - :success on successful completion
   - :error on failure"
  ...)

(defun parse-sse-chunk (line)
  "Parses a single SSE data line and extracts content delta.
   
   Parameters:
   - line: SSE data line (e.g., 'data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}')
   
   Returns:
   - Content string if present
   - nil if no content or [DONE] marker"
  ...)
```

**Implementation Strategy:**

There are two viable approaches for implementing streaming in Common Lisp:

**Approach A: Dexador with Stream Reading (Recommended)**
- Use Dexador's `:want-stream t` option to get raw HTTP response stream
- Wrap stream with flexi-streams for proper character encoding
- Read stream line-by-line, parsing SSE format (`data: ...` lines)
- Extract content from JSON delta objects using cl-json
- Invoke callback for each content chunk
- Handle `[DONE]` marker to detect completion
- Implement timeout and error handling

**Approach B: Direct Socket with usocket (Alternative)**
- Use usocket for low-level TCP connection
- Manually construct HTTP request with `stream: true` parameter
- Parse HTTP response headers
- Read SSE-formatted body line-by-line
- More control but more complex implementation

**Recommended: Approach A** - Dexador provides HTTP handling while allowing stream access, reducing complexity while maintaining flexibility.

**Key Implementation Details:**

```lisp
;; Example of streaming with Dexador
(let ((stream (dex:post url
                        :headers headers
                        :content request-body
                        :want-stream t
                        :keep-alive nil)))
  (unwind-protect
      (loop for line = (read-line stream nil nil)
            while line
            do (when (starts-with "data: " line)
                 (let ((chunk (parse-sse-chunk line)))
                   (when chunk
                     (funcall callback chunk)))))
    (close stream)))
```

#### 2. Streaming API Handlers (`src/api/handlers/*-handler.lisp`)

**New Functions for Each Feature:**

```lisp
(defun handle-monster-diagnostic-streaming (params)
  "Handles streaming monster diagnostic requests.
   
   Parameters:
   - params: Request parameters (language, inputs, etc.)
   
   Returns:
   - Clack streaming response with SSE format"
  ...)

(defun handle-character-chat-streaming (params)
  "Handles streaming character chat requests.
   
   Parameters:
   - params: Request parameters (language, character, messages, etc.)
   
   Returns:
   - Clack streaming response with SSE format"
  ...)
```

**SSE Response Format:**

```
Content-Type: text/event-stream
Cache-Control: no-cache
Connection: keep-alive

data: {"chunk": "Hello"}

data: {"chunk": " world"}

data: {"chunk": "!"}

data: {"done": true}

```

**Implementation Strategy:**

- Return Clack streaming response using lambda
- Set appropriate SSE headers
- Call streaming LLM service with callback
- Format each chunk as SSE data event
- Send `{"done": true}` marker on completion
- Handle errors by sending error event

#### 3. Streaming Utilities (`src/utils/streaming.lisp`)

**New Functions:**

```lisp
(defun create-sse-response (generator-fn)
  "Creates a Clack streaming response for SSE.
   
   Parameters:
   - generator-fn: Function that accepts a writer callback
   
   Returns:
   - Clack response list with streaming body"
  ...)

(defun format-sse-data (data)
  "Formats data as SSE data event.
   
   Parameters:
   - data: String or JSON-encodable data
   
   Returns:
   - Formatted SSE string with 'data: ' prefix and newlines"
  ...)

(defun format-sse-error (error-message)
  "Formats error as SSE error event.
   
   Parameters:
   - error-message: Error description string
   
   Returns:
   - Formatted SSE error event"
  ...)
```

### Frontend Components

#### 1. Streaming Manager (`public/js/streaming-manager.js`)

**New Module:**

```javascript
// Streaming Manager Module
// Handles SSE connections and progressive content display

class StreamingManager {
    constructor(targetElement, options = {}) {
        this.targetElement = targetElement;
        this.options = {
            onChunk: options.onChunk || this.defaultOnChunk.bind(this),
            onComplete: options.onComplete || (() => {}),
            onError: options.onError || this.defaultOnError.bind(this),
            updateInterval: options.updateInterval || 100, // ms
            ...options
        };
        this.buffer = '';
        this.eventSource = null;
        this.updateTimer = null;
        this.isActive = false;
    }

    start(url, params) {
        // Establish EventSource connection
        // Set up event listeners
        // Start buffered updates
        // Set isActive = true
        // Show streaming indicator
        // Disable submit buttons
    }

    stop(flush = true) {
        // Close EventSource immediately
        // Clear all timers
        // Flush buffer if requested
        // Set isActive = false
        // Hide streaming indicator
        // Re-enable submit buttons
        // Clean up all resources
    }

    destroy() {
        // Stop streaming if active
        // Release all resources
        // Clear all references
        // Prevent memory leaks
    }

    defaultOnChunk(chunk) {
        // Append to buffer
        // Schedule DOM update
    }

    flushBuffer() {
        // Update DOM with buffered content
        // Clear buffer
    }

    defaultOnError(error) {
        // Display error message
        // Clean up connection
        // Hide streaming indicator
        // Re-enable submit buttons
    }
}
```

**Key Features:**

- Manages EventSource lifecycle
- Buffers chunks for efficient DOM updates
- Throttles updates to maintain performance
- Provides hooks for custom rendering
- Handles connection errors and cleanup

#### 2. HTMX Integration

**Approach 1: HTMX SSE Extension (Recommended)**

Use HTMX's built-in SSE support:

```html
<div hx-ext="sse" 
     sse-connect="/api/character-chat-stream?character=dracula&language=en"
     sse-swap="message"
     hx-target="#chat-messages"
     hx-swap="beforeend">
</div>
```

**Approach 2: Custom JavaScript with HTMX**

Trigger HTMX updates from custom streaming code:

```javascript
function startStreaming(url, targetId) {
    const manager = new StreamingManager(
        document.getElementById(targetId),
        {
            onChunk: (chunk) => {
                // Update target element
                htmx.trigger(targetId, 'streaming-update', {chunk: chunk});
            }
        }
    );
    manager.start(url);
}
```

#### 3. UI Components Updates

**Loading Indicators:**

```html
<!-- Streaming indicator -->
<div class="streaming-indicator" style="display: none;">
    <span class="loading loading-dots loading-sm"></span>
    <span data-i18n="streaming">Generating response...</span>
    <button class="btn btn-sm btn-ghost" onclick="stopStreaming()">
        <span data-i18n="stop">Stop</span>
    </button>
</div>
```

**UI State Management:**

The streaming UI must properly manage state transitions:

1. **Before Streaming**
   - Streaming indicator: hidden
   - Submit buttons: enabled
   - Stop button: hidden

2. **During Streaming**
   - Streaming indicator: visible with animation
   - Submit buttons: disabled
   - Stop button: visible and functional
   - Content area: updating progressively

3. **After Streaming (Success)**
   - Streaming indicator: hidden
   - Submit buttons: re-enabled
   - Stop button: hidden
   - Content area: shows complete response

4. **After Streaming (Error)**
   - Streaming indicator: hidden
   - Submit buttons: re-enabled
   - Stop button: hidden
   - Error message: displayed with retry option

5. **After User Stop**
   - Streaming indicator: hidden immediately
   - Submit buttons: re-enabled immediately
   - Stop button: hidden
   - Content area: shows partial response

**State Management Functions:**

```javascript
function showStreamingIndicator() {
    const indicator = document.getElementById('streaming-indicator');
    if (indicator) {
        indicator.classList.add('active');
        indicator.style.display = 'flex';
    }
}

function hideStreamingIndicator() {
    const indicator = document.getElementById('streaming-indicator');
    if (indicator) {
        indicator.classList.remove('active');
        indicator.style.display = 'none';
    }
}

function disableSubmitButtons() {
    document.querySelectorAll('button[type="submit"]').forEach(btn => {
        btn.disabled = true;
    });
}

function enableSubmitButtons() {
    document.querySelectorAll('button[type="submit"]').forEach(btn => {
        btn.disabled = false;
    });
}

function stopStreaming() {
    // Stop all active streaming managers
    if (activeStreamingManager) {
        activeStreamingManager.stop(true); // Preserve partial content
        activeStreamingManager.destroy();
        activeStreamingManager = null;
    }
    
    // Reset UI state
    hideStreamingIndicator();
    enableSubmitButtons();
}
```

**Response Containers:**

```html
<!-- Streaming response container -->
<div id="streaming-response" class="streaming-content">
    <div class="chat chat-start">
        <div class="chat-bubble" id="streaming-bubble">
            <!-- Content appears here progressively -->
        </div>
    </div>
</div>
```

## Data Models

### Request Formats

#### Streaming Request Parameters

All streaming requests include standard parameters plus streaming flag:

```json
{
    "language": "en",
    "stream": true,
    "feature_specific_params": "..."
}
```

#### Character Chat Streaming Request

```json
{
    "language": "en",
    "character": "dracula",
    "messages": [
        {"role": "user", "content": "Hello"},
        {"role": "assistant", "content": "Good evening"}
    ],
    "stream": true
}
```

### Response Formats

#### SSE Chunk Format

```
data: {"chunk": "content piece", "type": "content"}

data: {"chunk": "", "type": "done"}

data: {"error": "error message", "type": "error"}

```

#### Parsed Chunk Object (Frontend)

```javascript
{
    chunk: "content piece",
    type: "content" | "done" | "error",
    error: "error message" // only for error type
}
```

### Internal Data Structures

#### Stream State (Backend)

```lisp
(defstruct stream-state
  (buffer "" :type string)           ; Accumulated content
  (chunk-count 0 :type integer)      ; Number of chunks received
  (start-time 0 :type integer)       ; Stream start timestamp
  (active-p t :type boolean)         ; Whether stream is active
  (error nil :type (or null string))) ; Error message if any
```

#### Stream Context (Frontend)

```javascript
{
    eventSource: EventSource,      // SSE connection
    buffer: "",                    // Content buffer
    targetElement: HTMLElement,    // DOM target
    startTime: Date,               // Stream start time
    chunkCount: 0,                 // Chunks received
    isActive: true,                // Connection status
    updateTimer: number            // Throttle timer ID
}
```

## Error Handling

### Error Categories

1. **Connection Errors**
   - Failed to establish SSE connection
   - Network timeout during streaming
   - Connection dropped mid-stream

2. **LLM Service Errors**
   - LLM API returns error status
   - Invalid stream format from LLM
   - LLM service doesn't support streaming

3. **Parsing Errors**
   - Malformed SSE data
   - Invalid JSON in chunk
   - Unexpected chunk format

4. **Client Errors**
   - Browser doesn't support EventSource
   - DOM update failures
   - Memory issues with large responses

### Error Handling Strategies

#### Backend Error Handling

```lisp
(defun handle-streaming-error (condition stream-state)
  "Handles errors during streaming.
   
   - Logs error details
   - Sends error event to client
   - Closes stream gracefully
   - Returns partial content if available"
  (log-error "Streaming error" (format nil "~A" condition))
  (when (stream-state-active-p stream-state)
    (send-sse-error (format nil "~A" condition))
    (send-sse-done))
  :error)
```

**Error Recovery:**

- Attempt to send error event before closing
- Preserve partial content received
- Log detailed error information
- Return appropriate HTTP status

#### Frontend Error Handling

```javascript
function handleStreamingError(error, context) {
    console.error('Streaming error:', error);
    
    // Display error to user
    showErrorMessage(getErrorMessage(error));
    
    // Preserve partial content
    if (context.buffer) {
        flushBuffer(context);
    }
    
    // Clean up connection
    if (context.eventSource) {
        context.eventSource.close();
    }
    
    // Offer retry option
    showRetryButton();
}
```

**Error Recovery:**

- Display user-friendly error message
- Show partial content if available
- Provide retry button
- Fall back to non-streaming mode

### Fallback Mechanisms

1. **Streaming Not Supported**
   - Detect lack of streaming support
   - Automatically use non-streaming endpoint
   - No error shown to user

2. **Streaming Fails**
   - Show error message
   - Offer retry with streaming
   - Provide option to use non-streaming mode

3. **Browser Compatibility**
   - Detect EventSource support
   - Use non-streaming mode for unsupported browsers
   - Polyfill if necessary

## Testing Strategy

### Unit Tests

#### Backend Tests (`tests/streaming-tests.lisp`)

```lisp
(def-suite streaming-tests
  :description "Tests for streaming LLM functionality")

(in-suite streaming-tests)

(test parse-sse-chunk-valid
  "Test parsing valid SSE chunks"
  (is (string= "hello" 
               (parse-sse-chunk "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}"))))

(test parse-sse-chunk-done
  "Test parsing [DONE] marker"
  (is (null (parse-sse-chunk "data: [DONE]"))))

(test streaming-callback-invocation
  "Test that callback is invoked for each chunk"
  (let ((chunks '()))
    (call-llm-streaming "test prompt"
                       (lambda (chunk) (push chunk chunks))
                       :mock-stream t)
    (is (= 3 (length chunks)))))

(test streaming-error-handling
  "Test error handling during streaming"
  (signals error
    (call-llm-streaming "test prompt"
                       (lambda (chunk) (error "Test error"))
                       :mock-stream t)))
```

#### Frontend Tests

Use Jest or similar for JavaScript unit tests:

```javascript
describe('StreamingManager', () => {
    test('buffers chunks correctly', () => {
        const manager = new StreamingManager(mockElement);
        manager.defaultOnChunk('hello');
        manager.defaultOnChunk(' world');
        expect(manager.buffer).toBe('hello world');
    });

    test('throttles DOM updates', (done) => {
        const manager = new StreamingManager(mockElement, {
            updateInterval: 50
        });
        manager.defaultOnChunk('test');
        expect(mockElement.innerHTML).toBe('');
        setTimeout(() => {
            expect(mockElement.innerHTML).toContain('test');
            done();
        }, 60);
    });
});
```

### Integration Tests

#### Backend Integration Tests

```lisp
(test streaming-api-endpoint
  "Test streaming API endpoint returns SSE format"
  (let ((response (dex:get "http://localhost:5000/api/character-chat-stream?character=dracula&message=hello"
                           :want-stream t)))
    (is (search "text/event-stream" (dex:response-headers response)))
    (is (search "data:" (read-line response)))))
```

### E2E Tests

#### Playwright Tests (`e2e-tests/streaming.spec.js`)

```javascript
const { test, expect } = require('@playwright/test');

test.describe('Streaming LLM Responses', () => {
    test('should display character chat response progressively', async ({ page }) => {
        await page.goto('http://localhost:5000');
        
        // Select character chat
        await page.click('[data-feature="character-chat"]');
        await page.selectOption('#chat-character', 'dracula');
        
        // Send message
        await page.fill('#chat-message-input', 'Tell me a story');
        
        // Track content updates
        const updates = [];
        page.on('domcontentloaded', () => {
            const content = page.locator('#streaming-bubble').textContent();
            updates.push(content);
        });
        
        await page.click('#chat-submit');
        
        // Wait for streaming to complete
        await page.waitForSelector('.streaming-indicator', { state: 'hidden' });
        
        // Verify progressive updates occurred
        expect(updates.length).toBeGreaterThan(1);
        
        // Verify final content is complete
        const finalContent = await page.locator('#streaming-bubble').textContent();
        expect(finalContent.length).toBeGreaterThan(50);
    });

    test('should handle stop button during streaming', async ({ page }) => {
        await page.goto('http://localhost:5000');
        
        await page.click('[data-feature="character-chat"]');
        await page.fill('#chat-message-input', 'Tell me a long story');
        await page.click('#chat-submit');
        
        // Wait for streaming to start
        await page.waitForSelector('.streaming-indicator');
        
        // Click stop button
        await page.click('.streaming-indicator button');
        
        // Verify streaming stopped
        await expect(page.locator('.streaming-indicator')).toBeHidden();
        
        // Verify partial content is preserved
        const content = await page.locator('#streaming-bubble').textContent();
        expect(content.length).toBeGreaterThan(0);
    });

    test('should fall back to non-streaming on error', async ({ page }) => {
        // Mock streaming failure
        await page.route('**/api/character-chat-stream*', route => {
            route.abort();
        });
        
        await page.goto('http://localhost:5000');
        await page.click('[data-feature="character-chat"]');
        await page.fill('#chat-message-input', 'Hello');
        await page.click('#chat-submit');
        
        // Verify error message shown
        await expect(page.locator('.alert-error')).toBeVisible();
        
        // Verify retry option available
        await expect(page.locator('button:has-text("Retry")')).toBeVisible();
    });
});
```

### Performance Tests

```javascript
test('should maintain performance with rapid chunks', async ({ page }) => {
    await page.goto('http://localhost:5000');
    
    // Monitor performance
    const metrics = await page.evaluate(() => {
        return new Promise((resolve) => {
            const observer = new PerformanceObserver((list) => {
                const entries = list.getEntries();
                resolve(entries);
            });
            observer.observe({ entryTypes: ['measure'] });
            
            // Trigger streaming
            // ...
        });
    });
    
    // Verify no long tasks
    const longTasks = metrics.filter(m => m.duration > 50);
    expect(longTasks.length).toBe(0);
});
```

## Implementation Notes

### Integration with Existing Code

#### File Organization

```
src/
├── services/
│   ├── llm-service.lisp          # Add streaming functions here
│   └── streaming-service.lisp    # New: Streaming utilities
├── api/
│   └── handlers/
│       ├── character-chat-handler.lisp  # Add streaming handler
│       ├── monster-diagnostic-handler.lisp
│       └── ...
├── utils/
│   └── streaming.lisp            # New: SSE formatting utilities
└── main.lisp                     # Add streaming routes

public/
├── js/
│   ├── streaming-manager.js      # New: Streaming client
│   ├── chat-manager.js           # Update for streaming
│   └── feature-manager.js        # Update for streaming
└── index.html                    # Add streaming UI elements
```

#### ASDF System Definition Updates

```lisp
;; kabotan.asd
:serial t
:components ((:file "package")
             (:file "utils/logging")
             (:file "utils/validation")
             (:file "utils/error-handling")
             (:file "utils/response-formatting")
             (:file "utils/streaming")           ; New
             (:file "services/llm-service")
             (:file "services/streaming-service") ; New
             (:file "services/language-handler")
             (:file "services/prompt-builder")
             (:file "api/handlers/character-chat-handler")
             ; ... other handlers
             (:file "api/halloween-api")
             (:file "main"))
```

### Configuration

#### Environment Variables

```bash
# Enable/disable streaming
ENABLE_STREAMING=true

# Streaming timeout (seconds)
STREAMING_TIMEOUT=360

# Chunk buffer size
STREAMING_BUFFER_SIZE=1024

# Update throttle interval (ms)
STREAMING_UPDATE_INTERVAL=100
```

#### Runtime Configuration

```lisp
(defvar *streaming-enabled* t
  "Whether streaming is enabled globally")

(defvar *streaming-timeout* 360
  "Timeout for streaming requests in seconds")

(defvar *streaming-buffer-size* 1024
  "Buffer size for streaming chunks")
```

### Dependencies

#### Backend Dependencies

Existing dependencies (already in use):
- `dexador` - HTTP client with `:want-stream t` support
- `cl-json` - JSON parsing
- `clack` - HTTP server with streaming response support

New dependencies (need to add to kabotan.asd):
- `flexi-streams` - For proper character encoding when reading streams
- `bordeaux-threads` (optional) - For async streaming if needed

**Note on Dexador Streaming:**
Dexador's `:want-stream t` option returns the underlying stream from the HTTP response, allowing us to read it incrementally. This is sufficient for implementing SSE client functionality. The stream can be read line-by-line to parse SSE format.

#### Frontend Dependencies

No new dependencies required - use existing:
- HTMX - Already included, has SSE extension
- Native EventSource API - Built into modern browsers

### Backward Compatibility

1. **Dual Endpoints**
   - Keep existing non-streaming endpoints
   - Add new streaming endpoints with `-stream` suffix
   - Example: `/api/character-chat` and `/api/character-chat-stream`

2. **Feature Detection**
   - Frontend detects streaming support
   - Falls back to non-streaming automatically
   - No user configuration needed

3. **Configuration Flag**
   - `ENABLE_STREAMING` environment variable
   - Defaults to `true` for new deployments
   - Can be disabled for compatibility

### Migration Path

1. **Phase 1**: Implement streaming infrastructure
   - Add streaming utilities
   - Add streaming LLM service functions
   - Add SSE formatting functions

2. **Phase 2**: Add streaming endpoints
   - Implement streaming handlers for each feature
   - Add streaming routes
   - Keep existing endpoints unchanged

3. **Phase 3**: Update frontend
   - Add streaming manager
   - Update UI components
   - Add feature detection

4. **Phase 4**: Testing and optimization
   - Run E2E tests
   - Performance testing
   - Error handling verification

5. **Phase 5**: Gradual rollout
   - Enable for subset of users
   - Monitor performance and errors
   - Full rollout after validation

