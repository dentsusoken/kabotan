# Design Document

## Overview

This design document outlines enhancements to the LLM service infrastructure to improve observability, user experience, and API capabilities. The enhancements include structured logging, frontend loading indicators, conversation history support, and system prompt configuration.

## Architecture

### High-Level Architecture

```
┌─────────────────┐
│   Frontend      │
│   (HTMX)        │
│                 │
│ - Loading UI    │
│ - History Mgmt  │
└────────┬────────┘
         │
         │ HTTP POST
         │
┌────────▼────────┐
│   API Layer     │
│  (Ningle)       │
│                 │
│ - Validation    │
│ - Error Handle  │
└────────┬────────┘
         │
         │
┌────────▼────────┐
│  LLM Service    │
│                 │
│ - Logging       │
│ - Message API   │
│ - Retry Logic   │
└────────┬────────┘
         │
         │ OpenAI API
         │
┌────────▼────────┐
│  External LLM   │
│  (OpenAI-compat)│
└─────────────────┘
```

## Components and Interfaces

### 1. Enhanced Logging System

#### Component: Logger Module

**Location**: `src/utils/logging.lisp`

**Purpose**: Provide structured logging for all LLM service operations

**Interface**:
```lisp
(defun log-llm-request (model prompt-info config)
  "Log LLM API request details")

(defun log-llm-response (status-code response-info timing)
  "Log LLM API response details")

(defun log-llm-error (error-type error-message context)
  "Log LLM API errors with context")

(defun log-llm-retry (attempt backoff-seconds reason)
  "Log retry attempts")
```

**Log Format**:
```
[TIMESTAMP] [LEVEL] [COMPONENT] message
Example:
[2024-11-13T10:30:45] [INFO] [LLM-REQUEST] Model: gemma3-12b, Prompt: 150 chars, Tokens: 1000, Temp: 0.7
[2024-11-13T10:30:47] [INFO] [LLM-RESPONSE] Status: 200, Response: 450 chars, Time: 2.3s
[2024-11-13T10:30:50] [ERROR] [LLM-ERROR] Timeout after 30s, Context: character-chat
[2024-11-13T10:30:51] [WARN] [LLM-RETRY] Attempt 1/2, Backoff: 1s, Reason: timeout
```

**Design Decisions**:
- Use `*error-output*` stream for all logs to separate from application output
- Include timestamps using `get-universal-time` and format as ISO 8601
- Structured format for easy parsing by log aggregation tools
- Severity levels: INFO, WARN, ERROR
- Component tags for filtering: LLM-REQUEST, LLM-RESPONSE, LLM-ERROR, LLM-RETRY

### 2. Frontend Loading Indicators

#### Component: HTMX Loading States

**Location**: `public/index.html` (inline JavaScript and CSS)

**Purpose**: Provide visual feedback during API requests

**Implementation Strategy**:
- Use HTMX's built-in `htmx-indicator` class
- Add loading spinners to submit buttons
- Disable form inputs during requests
- Show/hide loading state automatically via HTMX events

**HTML Structure**:
```html
<button type="submit" class="btn btn-primary">
  <span>Submit</span>
  <span id="loading-indicator" class="loading loading-spinner htmx-indicator"></span>
</button>
```

**CSS**:
```css
.htmx-indicator {
  display: none;
}

.htmx-request .htmx-indicator {
  display: inline-block;
}

.htmx-request button {
  opacity: 0.7;
  pointer-events: none;
}
```

**HTMX Configuration**:
- Use `hx-indicator` attribute to specify loading element
- Leverage `htmx:beforeRequest` and `htmx:afterRequest` events
- Add `hx-disabled-elt` to disable form during submission

### 3. Message History Support

#### Component: Message-Based LLM API

**Location**: `src/services/llm-service.lisp`

**Purpose**: Support conversation context with message arrays

**Interface**:
```lisp
(defun call-llm-with-messages (messages &key (max-tokens 1000) (temperature 0.7) (timeout 30) system-prompt)
  "Call LLM with message history
   
   Parameters:
   - messages: List of message plists with :role and :content
               Example: ((:role \"user\" :content \"Hello\") (:role \"assistant\" :content \"Hi!\"))
   - system-prompt: Optional system message (string)
   - max-tokens, temperature, timeout: Same as existing API
   
   Returns:
   - Response text string on success
   - nil on failure")
```

**Message Format**:
```lisp
;; Input format
(list
  (:role "system" :content "You are a helpful assistant")
  (:role "user" :content "What is Halloween?")
  (:role "assistant" :content "Halloween is a holiday...")
  (:role "user" :content "When is it celebrated?"))

;; API request format (JSON)
{
  "model": "gemma3-12b",
  "messages": [
    {"role": "system", "content": "You are a helpful assistant"},
    {"role": "user", "content": "What is Halloween?"},
    {"role": "assistant", "content": "Halloween is a holiday..."},
    {"role": "user", "content": "When is it celebrated?"}
  ],
  "max_tokens": 1000,
  "temperature": 0.7
}
```

**Design Decisions**:
- Messages are plists with `:role` and `:content` keys
- Valid roles: "system", "user", "assistant"
- System prompt is automatically prepended as first message if provided
- Backward compatibility: existing `call-llm` function remains unchanged
- New function `call-llm-with-messages` for message-based API

#### Component: Frontend History Management

**Location**: `public/index.html` (JavaScript)

**Purpose**: Maintain conversation state in browser

**Implementation**:
```javascript
// Conversation history per feature
const conversationHistory = {
  'character-chat': [],
  'trivia-bot': []
};

// Add message to history
function addToHistory(feature, role, content) {
  conversationHistory[feature].push({
    role: role,
    content: content
  });
}

// Clear history
function clearHistory(feature) {
  conversationHistory[feature] = [];
}

// Get history for API call
function getHistory(feature) {
  return conversationHistory[feature];
}
```

**API Integration**:
- Send full conversation history with each request
- Use JSON format for message arrays
- Update history after receiving response
- Persist history in sessionStorage for page refreshes

### 4. System Prompt Configuration

#### Component: System Prompt Parameter

**Location**: `src/services/llm-service.lisp` and prompt builders

**Purpose**: Allow features to configure LLM behavior

**Interface**:
```lisp
;; In llm-service.lisp
(defun call-llm-with-messages (messages &key system-prompt ...)
  "System prompt is prepended as first message if provided")

;; In prompt builders
(defun build-character-chat-prompt-with-system (character language)
  "Return system prompt for character chat
   
   Returns: String containing system-level instructions")
```

**System Prompt Examples**:
```lisp
;; Character Chat - Dracula
"You are Count Dracula, the legendary vampire. Speak in a formal, aristocratic manner with occasional references to your castle and nocturnal lifestyle. Be charming but slightly menacing."

;; Trivia Bot
"You are a Halloween trivia expert. Provide accurate, educational information about Halloween history, traditions, and folklore. Always include an interesting fact in your responses."

;; Story Generator
"You are a creative Halloween storyteller. Generate engaging, atmospheric stories with vivid descriptions and appropriate pacing for the chosen style (gothic, parody, or classic)."
```

**Design Decisions**:
- System prompts are feature-specific
- Stored as constants or functions in prompt-builder.lisp
- Can be overridden per request if needed
- Automatically included as first message in API calls

## Data Models

### Log Entry Structure

```lisp
(defstruct log-entry
  timestamp      ; Universal time
  level          ; :info, :warn, :error
  component      ; :llm-request, :llm-response, :llm-error, :llm-retry
  message        ; String
  context)       ; Plist with additional data
```

### Message Structure

```lisp
;; Plist format
(:role "user" :content "Hello, how are you?")

;; Valid roles
"system"    ; System-level instructions
"user"      ; User messages
"assistant" ; LLM responses
```

### API Request Structure (Enhanced)

```lisp
;; New message-based format
{
  "model": "gemma3-12b",
  "messages": [
    {"role": "system", "content": "..."},
    {"role": "user", "content": "..."},
    {"role": "assistant", "content": "..."}
  ],
  "max_tokens": 1000,
  "temperature": 0.7
}

;; Legacy prompt-based format (still supported)
{
  "model": "gemma3-12b",
  "messages": [
    {"role": "user", "content": "..."}
  ],
  "max_tokens": 1000,
  "temperature": 0.7
}
```

## Error Handling

### Enhanced Error Logging

**Strategy**: Log all errors with full context before returning to caller

**Implementation**:
```lisp
(handler-case
    (call-openai-api-with-messages messages ...)
  (dex:http-request-timeout (condition)
    (log-llm-error :timeout 
                   (format nil "Request timeout after ~As" timeout)
                   `(:messages-count ,(length messages) :timeout ,timeout))
    nil)
  (dex:http-request-failed (condition)
    (log-llm-error :http-error
                   (format nil "HTTP ~A: ~A" 
                           (dex:response-status condition)
                           (dex:response-body condition))
                   `(:status ,(dex:response-status condition)))
    nil)
  (error (condition)
    (log-llm-error :unknown
                   (format nil "~A" condition)
                   nil)
    nil))
```

### Frontend Error Display

**Strategy**: Show user-friendly error messages with retry options

**Implementation**:
- Parse error responses from backend
- Display in alert components
- Provide "Retry" button for transient errors
- Log errors to browser console for debugging

## Testing Strategy

### Unit Tests

**Location**: `tests/llm-service-tests.lisp`

**Coverage**:
1. Logging functions
   - Test log format and output
   - Verify timestamp formatting
   - Check severity levels

2. Message formatting
   - Test message array conversion
   - Verify system prompt prepending
   - Check role validation

3. API call functions
   - Mock HTTP responses
   - Test message-based API
   - Verify backward compatibility

### Integration Tests

**Location**: `e2e-tests/conversation-history.spec.js`

**Coverage**:
1. Multi-turn conversations
   - Send multiple messages
   - Verify context retention
   - Check response relevance

2. Loading indicators
   - Verify spinner visibility
   - Check button disable state
   - Test error state transitions

3. System prompts
   - Test character consistency
   - Verify behavior changes
   - Check language handling

### Manual Testing

**Checklist**:
- [ ] Verify logs appear in console during API calls
- [ ] Check loading spinner shows/hides correctly
- [ ] Test multi-turn conversation flow
- [ ] Verify system prompt affects responses
- [ ] Test error scenarios (timeout, invalid input)
- [ ] Check mobile responsiveness of loading states

## Migration Strategy

### Backward Compatibility

**Approach**: Maintain existing `call-llm` function, add new `call-llm-with-messages`

**Implementation**:
```lisp
;; Existing function - unchanged
(defun call-llm (prompt &key ...)
  "Legacy prompt-based API")

;; New function - message-based
(defun call-llm-with-messages (messages &key ...)
  "New message-based API")

;; Internal helper - converts prompt to message format
(defun prompt-to-messages (prompt)
  "Convert single prompt string to message array"
  (list (:role "user" :content prompt)))
```

### Gradual Rollout

**Phase 1**: Add logging and loading indicators
- Enhance existing endpoints with logging
- Add loading spinners to all forms
- No API changes required

**Phase 2**: Add message-based API
- Implement `call-llm-with-messages`
- Keep existing endpoints using `call-llm`
- Add new endpoints for conversation features

**Phase 3**: Migrate existing features
- Update character-chat to use message history
- Update trivia-bot to use message history
- Add system prompts to all features

## Performance Considerations

### Logging Performance

**Impact**: Minimal - logging is synchronous but fast
**Mitigation**: 
- Use `format` with `*error-output*` (buffered)
- Avoid complex string operations in hot paths
- Consider async logging for high-volume scenarios

### Message History Size

**Impact**: Larger request payloads with conversation history
**Mitigation**:
- Limit history to last N messages (e.g., 10)
- Implement history truncation in frontend
- Monitor token usage and adjust limits

### Frontend State Management

**Impact**: Memory usage for conversation history
**Mitigation**:
- Use sessionStorage for persistence
- Clear history on tab close
- Implement history size limits

## Security Considerations

### Input Validation

**Strategy**: Validate all message content before sending to LLM

**Implementation**:
- Sanitize message content (existing `sanitize-input`)
- Validate role values (whitelist: system, user, assistant)
- Check message array length limits
- Validate system prompt format

### Logging Sensitive Data

**Strategy**: Avoid logging full message content

**Implementation**:
- Log message count and character lengths only
- Truncate long messages in logs (first 50 chars)
- Never log API keys or tokens
- Redact user PII in logs

## Deployment Considerations

### Configuration

**Environment Variables** (no changes):
- `OPENAI_API_KEY`: API authentication
- `OPENAI_MODEL`: Model name
- `OPENAI_HOST`: API endpoint URL

**New Configuration** (optional):
- `LOG_LEVEL`: Control logging verbosity (INFO, WARN, ERROR)
- `MAX_HISTORY_LENGTH`: Limit conversation history size

### Monitoring

**Metrics to Track**:
- API request count and latency
- Error rate by error type
- Retry attempt frequency
- Average conversation length

**Log Aggregation**:
- Collect logs from `*error-output*`
- Parse structured log format
- Create dashboards for key metrics
- Set up alerts for error spikes

## Open Questions

1. Should we implement log rotation or rely on container log management?
   - **Decision**: Rely on container/system log management for now

2. What is the optimal conversation history length limit?
   - **Decision**: Start with 10 messages, adjust based on token usage

3. Should system prompts be configurable via environment variables?
   - **Decision**: No, keep them in code for version control and consistency

4. Do we need to persist conversation history across page refreshes?
   - **Decision**: Yes, use sessionStorage for basic persistence
