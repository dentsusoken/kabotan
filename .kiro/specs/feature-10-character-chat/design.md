# Design Document

## Overview

The Character Chat feature enables users to have conversations with Halloween character personas, each with distinct personalities and speaking styles.

## API Endpoint

### POST `/api/character-chat`

**Request Format:**
```json
{
  "character": "dracula|witch|jack",
  "message": "string",
  "language": "ja|en",
  "history": ["array of previous messages (optional)"]
}
```

**Response Format:**
```html
<div class="chat chat-start">
  <div class="chat-image avatar">
    <div class="w-10 rounded-full">
      <!-- Character icon -->
    </div>
  </div>
  <div class="chat-header">Character Name</div>
  <div class="chat-bubble">Character response...</div>
</div>
```

## Frontend Component

### Character Chat Component

**UI Elements:**
- Character selector (DaisyUI radio buttons or tabs):
  - Dracula (vampire icon)
  - Witch (witch hat icon)
  - Jack-o'-Lantern (pumpkin icon)
- Chat history display (DaisyUI chat bubbles)
- Message input (DaisyUI textarea)
- Send button (DaisyUI primary button)

**HTMX Integration:**
- `hx-post="/api/character-chat"`
- `hx-target="#chat-history"`
- `hx-swap="beforeend"` (append to chat history)
- `hx-indicator="#loading-spinner"`
- Include selected character in request

## Backend Implementation

### Prompt Builder Function

```lisp
(defun build-character-chat-prompt (character message language &optional history)
  "Builds a prompt for character-based chat responses.
   
   Parameters:
   - character: keyword (:dracula, :witch, :jack)
   - message: string (user message)
   - language: :ja or :en
   - history: list of previous messages (optional)
   
   Returns: string prompt for LLM")
```

**Character Personas:**

**Dracula:**
- Formal, aristocratic speech
- References to centuries of existence
- Sophisticated vocabulary
- Occasional dramatic flair
- Hints of dark humor

**Witch:**
- Mystical, cryptic speech
- References to spells and potions
- Playful and mischievous
- Uses magical terminology
- Wise but whimsical

**Jack-o'-Lantern:**
- Cheerful, enthusiastic
- Pumpkin-related puns
- Friendly and welcoming
- Halloween spirit embodied
- Warm and approachable

### API Handler

```lisp
(defun handle-character-chat (request)
  "Handles POST /api/character-chat endpoint.
   
   1. Parse and validate request parameters
   2. Validate character selection
   3. Build prompt with character persona
   4. Call LLM service
   5. Format response as chat bubble HTML
   6. Handle errors gracefully")
```

## Validation Rules

- Character must be one of: "dracula", "witch", "jack"
- Message must be non-empty string (max 500 characters)
- Language must be "ja" or "en"
- History is optional (max 10 previous messages)
- Input sanitization applied

## Chat History Management

- Client-side: Store chat history in DOM
- Server-side: Stateless (history passed in request if needed)
- History format: Array of {role, content} objects
- Limit history to last 10 messages for context window

## Radio Button Styling

### Visual Design
- Radio buttons must be visible against dark backgrounds
- Use light-colored borders (white or light gray) for radio button outlines
- Selected state should use primary color (purple/orange gradient)
- Unselected state should have visible border with transparent or dark fill
- Hover state should provide visual feedback

### CSS Implementation
```css
.radio {
    border-color: rgba(255, 255, 255, 0.5);
    background-color: transparent;
}

.radio:checked {
    border-color: var(--halloween-purple);
    background-color: var(--halloween-purple);
}

.radio:hover {
    border-color: rgba(255, 255, 255, 0.8);
}
```

## Error Handling

- Missing required fields: Return validation error
- Invalid character: Return validation error
- LLM service timeout: Return error with retry option
- System errors: Log and return generic error

## Testing Strategy

### Unit Tests
- Test prompt builder for each character
- Test character validation
- Test API handler with mocked LLM service
- Test history handling

### E2E Tests
- Test chat with each character
- Test multiple message exchanges
- Test character switching
- Test with Japanese and English
- Test error handling
