# Design Document

## Overview

The Monster Diagnostic feature analyzes user personality inputs and returns a matching Halloween monster type with a personalized description.

## API Endpoint

### POST `/api/monster-diagnostic`

**Request Format:**
```json
{
  "favorite_food": "string",
  "sleep_schedule": "string",
  "hobbies": "string",
  "language": "ja|en"
}
```

**Response Format:**
```html
<div class="feature-response card bg-base-200">
  <div class="card-body">
    <h3 class="card-title">Monster Type</h3>
    <p>Personality description...</p>
  </div>
</div>
```

## Frontend Component

### Monster Diagnostic Component

**UI Elements:**
- DaisyUI form with input fields:
  - Favorite food (text input)
  - Sleep schedule (text input)
  - Hobbies (textarea)
- Submit button (DaisyUI primary button with Halloween theme)
- Result display area (DaisyUI card)

**HTMX Integration:**
- `hx-post="/api/monster-diagnostic"`
- `hx-target="#result-area"`
- `hx-swap="innerHTML"`
- `hx-indicator="#loading-spinner"`

## Backend Implementation

### Prompt Builder Function

```lisp
(defun build-monster-diagnostic-prompt (inputs language)
  "Builds a prompt for monster personality diagnosis based on user inputs.
   
   Parameters:
   - inputs: alist with keys :favorite-food, :sleep-schedule, :hobbies
   - language: :ja or :en
   
   Returns: string prompt for LLM")
```

**Prompt Structure:**
- System instruction: Act as a Halloween personality analyst
- User inputs: Include all provided personality information
- Output format: Monster type name + personality description
- Language instruction: Respond in specified language
- Halloween theme: Maintain festive, fun tone

### API Handler

```lisp
(defun handle-monster-diagnostic (request)
  "Handles POST /api/monster-diagnostic endpoint.
   
   1. Parse and validate request parameters
   2. Build prompt using prompt-builder
   3. Call LLM service
   4. Format response as HTML
   5. Handle errors gracefully")
```

## Validation Rules

- All input fields must be non-empty strings
- Language must be "ja" or "en"
- Input sanitization applied to prevent injection attacks
- Maximum input length: 500 characters per field

## Error Handling

- Missing required fields: Return validation error message
- LLM service timeout: Return user-friendly error with retry option
- Invalid language: Default to English
- System errors: Log and return generic error message

## Testing Strategy

### Unit Tests
- Test prompt builder with various input combinations
- Test validation functions
- Test API handler with mocked LLM service

### E2E Tests
- Test complete flow from form submission to result display
- Test with Japanese and English languages
- Test error handling when LLM service is unavailable
- Test input validation
