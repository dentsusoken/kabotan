# Design Document

## Overview

The Story Generator feature creates personalized Halloween stories based on user inputs and selected narrative style.

## API Endpoint

### POST `/api/story-generator`

**Request Format:**
```json
{
  "name": "string",
  "style": "gothic|parody|classic",
  "elements": "string",
  "language": "ja|en"
}
```

**Response Format:**
```html
<div class="feature-response card bg-base-200">
  <div class="card-body prose prose-invert">
    <h3 class="card-title">Story Title</h3>
    <div class="story-content">
      Story narrative...
    </div>
  </div>
</div>
```

## Frontend Component

### Story Generator Component

**UI Elements:**
- DaisyUI form with input fields:
  - Name (text input)
  - Story elements/theme (textarea)
  - Style selector (DaisyUI select dropdown or radio buttons)
    - Gothic
    - Parody
    - Classic Ghost Story
- Generate button (DaisyUI primary button with Halloween theme)
- Story display area (DaisyUI card with prose styling)

**HTMX Integration:**
- `hx-post="/api/story-generator"`
- `hx-target="#story-area"`
- `hx-swap="innerHTML"`
- `hx-indicator="#loading-spinner"`

## Backend Implementation

### Prompt Builder Function

```lisp
(defun build-story-generator-prompt (name style elements language)
  "Builds a prompt for story generation with specified style.
   
   Parameters:
   - name: string (character name)
   - style: keyword (:gothic, :parody, :classic)
   - elements: string (story elements/theme)
   - language: :ja or :en
   
   Returns: string prompt for LLM")
```

**Prompt Structure:**
- System instruction: Act as a Halloween story writer
- Style-specific instructions:
  - Gothic: Dark, atmospheric, Victorian-era inspired
  - Parody: Humorous, lighthearted, playful
  - Classic: Traditional ghost story, suspenseful
- User inputs: Character name and story elements
- Output format: Title + narrative text
- Language instruction: Write in specified language
- Length guidance: 300-500 words

### API Handler

```lisp
(defun handle-story-generator (request)
  "Handles POST /api/story-generator endpoint.
   
   1. Parse and validate request parameters
   2. Validate story style
   3. Build prompt using prompt-builder
   4. Call LLM service with higher max tokens
   5. Format response as HTML with prose styling
   6. Handle errors gracefully")
```

## Validation Rules

- Name must be non-empty string (max 100 characters)
- Style must be one of: "gothic", "parody", "classic"
- Elements must be non-empty string (max 500 characters)
- Language must be "ja" or "en"
- Input sanitization applied

## Story Styles

### Gothic
- Dark, atmospheric tone
- Victorian-era inspired elements
- Emphasis on mood and suspense
- Poetic language

### Parody
- Humorous, lighthearted approach
- Playful subversion of horror tropes
- Comedic timing and situations
- Fun, accessible tone

### Classic Ghost Story
- Traditional ghost story structure
- Suspenseful buildup
- Supernatural elements
- Timeless narrative style

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
- Invalid style: Return validation error
- LLM service timeout: Return error with retry option
- System errors: Log and return generic error

## Testing Strategy

### Unit Tests
- Test prompt builder for each style
- Test style validation
- Test API handler with mocked LLM service

### E2E Tests
- Test story generation for each style
- Test with Japanese and English
- Test with various name and element combinations
- Test error handling
