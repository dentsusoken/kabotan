# Design Document

## Overview

The Trivia Bot feature provides conversational responses that include interesting Halloween facts, educating users while entertaining them.

## API Endpoint

### POST `/api/trivia-bot`

**Request Format:**
```json
{
  "message": "string",
  "language": "ja|en"
}
```

**Response Format:**
```html
<div class="feature-response">
  <div class="chat chat-start">
    <div class="chat-bubble">
      Regular response text...
    </div>
  </div>
  <div class="alert alert-info mt-2">
    <svg>...</svg>
    <span><strong>Did you know?</strong> Trivia fact here...</span>
  </div>
</div>
```

## Frontend Component

### Trivia Bot Component

**UI Elements:**
- Conversation input (DaisyUI textarea)
- Send button (DaisyUI primary button)
- Response display area with:
  - Regular conversation text (DaisyUI chat bubble)
  - Highlighted trivia facts (DaisyUI alert/badge)

**HTMX Integration:**
- `hx-post="/api/trivia-bot"`
- `hx-target="#trivia-area"`
- `hx-swap="beforeend"`
- `hx-indicator="#loading-spinner"`

## Backend Implementation

### Prompt Builder Function

```lisp
(defun build-trivia-bot-prompt (message language)
  "Builds a prompt that includes Halloween trivia in responses.
   
   Parameters:
   - message: string (user message)
   - language: :ja or :en
   
   Returns: string prompt for LLM")
```

**Prompt Structure:**
- System instruction: Act as a Halloween trivia expert
- Trivia inclusion requirement: Include at least one interesting fact
- Trivia topics: Origins, cultural practices, international customs, symbols, traditions
- Response format: Conversational response + trivia fact
- Language instruction: Respond in specified language
- Tone: Educational but entertaining

### API Handler

```lisp
(defun handle-trivia-bot (request)
  "Handles POST /api/trivia-bot endpoint.
   
   1. Parse and validate request parameters
   2. Build prompt with trivia instructions
   3. Call LLM service
   4. Parse response to separate conversation and trivia
   5. Format response with highlighted trivia
   6. Handle errors gracefully")
```

## Trivia Topics

The trivia bot should cover diverse topics:
- **Origins**: Celtic Samhain, Christian All Hallows' Eve
- **Cultural Practices**: Trick-or-treating, costume traditions
- **International Customs**: How different countries celebrate
- **Symbols**: Jack-o'-lanterns, black cats, bats, witches
- **Traditions**: Apple bobbing, haunted houses, candy corn
- **History**: Evolution of Halloween over centuries
- **Folklore**: Supernatural beliefs, ghost stories

## Validation Rules

- Message must be non-empty string (max 500 characters)
- Language must be "ja" or "en"
- Input sanitization applied

## Response Formatting

### Trivia Highlighting
- Use DaisyUI alert component with info styling
- Include "Did you know?" prefix
- Use distinct color (orange or purple accent)
- Add icon for visual interest

### Response Structure
1. Conversational response to user message
2. Separated trivia fact section
3. Both styled for dark mode

### Text Contrast and Visibility
- Chat bubbles must have sufficient text contrast
- `.chat-bubble-accent` class should use light text on dark background
- `.chat-bubble-secondary` class should use light text on dark background
- Ensure white or light-colored text (`color: white !important`) for readability
- Apply consistent styling across all chat bubble variants

## Error Handling

- Missing message: Return validation error
- LLM service timeout: Return error with retry option
- No trivia in response: Log warning, display response anyway
- System errors: Log and return generic error

## Testing Strategy

### Unit Tests
- Test prompt builder includes trivia instructions
- Test response parsing
- Test API handler with mocked LLM service

### E2E Tests
- Test trivia bot conversation flow
- Test trivia facts are displayed
- Test with Japanese and English
- Test various conversation topics
- Test error handling
