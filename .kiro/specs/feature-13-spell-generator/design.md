# Design Document

## Overview

The Spell Generator feature creates magical phrases or spells with explanations, allowing users to regenerate new spells on demand.

## API Endpoint

### POST `/api/spell-generator`

**Request Format:**
```json
{
  "language": "ja|en"
}
```

**Response Format:**
```html
<div class="feature-response card bg-base-200">
  <div class="card-body">
    <h3 class="spell-phrase text-2xl font-bold text-primary">
      Magical phrase here...
    </h3>
    <p class="spell-explanation mt-4">
      Explanation of the spell...
    </p>
    <div class="card-actions justify-end mt-4">
      <button class="btn btn-primary" 
              hx-post="/api/spell-generator" 
              hx-target="#spell-area"
              hx-swap="innerHTML"
              hx-indicator="#loading-spinner">
        Regenerate
      </button>
    </div>
  </div>
</div>
```

## Frontend Component

### Spell Generator Component

**UI Elements:**
- Spell display area (DaisyUI card):
  - Magical phrase (large, decorative text)
  - Explanation (smaller text)
  - Regenerate button (DaisyUI primary button)
- Loading indicator during regeneration

**HTMX Integration:**
- Initial load: `hx-post="/api/spell-generator"` with `hx-trigger="load"`
- Regenerate button: `hx-post="/api/spell-generator"`
- `hx-target="#spell-area"`
- `hx-swap="innerHTML"`
- `hx-indicator="#loading-spinner"`

## Backend Implementation

### Prompt Builder Function

```lisp
(defun build-spell-generator-prompt (language)
  "Builds a prompt for generating a magical phrase or spell.
   
   Parameters:
   - language: :ja or :en
   
   Returns: string prompt for LLM")
```

**Prompt Structure:**
- System instruction: Act as a mystical spell creator
- Output format: Magical phrase + explanation
- Spell characteristics:
  - Halloween-themed
  - Mystical and atmospheric
  - Creative and unique
  - Can be protective, transformative, or celebratory
- Language instruction: Generate in specified language
- Length: Phrase (1-2 lines) + explanation (2-3 sentences)

**Language-Specific Prompt Design:**

For Japanese (ja):
- Entire prompt should be written in Japanese
- Instructions should guide the LLM to think and create in Japanese from the start
- Avoid English instructions that might cause translation behavior
- Use natural Japanese phrasing for mystical/magical context

For English (en):
- Entire prompt should be written in English
- Instructions should guide the LLM to create authentic English magical phrases
- Use English mystical terminology and phrasing conventions

### API Handler

```lisp
(defun handle-spell-generator (request)
  "Handles POST /api/spell-generator endpoint.
   
   1. Parse and validate request parameters
   2. Build prompt for spell generation
   3. Call LLM service
   4. Format response with spell and explanation
   5. Include regenerate button in response
   6. Handle errors gracefully")
```

## Spell Types

The spell generator should create diverse spell types:
- **Protective Spells**: Ward off evil spirits
- **Transformation Spells**: Change appearance or form
- **Summoning Spells**: Call upon Halloween spirits
- **Celebration Spells**: Enhance Halloween festivities
- **Fortune Spells**: Reveal future or grant wishes
- **Nature Spells**: Control elements or seasons

## Validation Rules

- Language must be "ja" or "en"
- No user input required (spell is randomly generated)

## Response Formatting

### Spell Display
- Phrase: Large, decorative font with primary color
- Explanation: Regular font, readable size
- Card layout with dark mode styling
- Regenerate button prominently placed

### Regeneration Behavior
- Button triggers new API call
- Loading indicator shown during generation
- New spell replaces old spell (innerHTML swap)
- No page reload required

## Error Handling

- LLM service timeout: Return error with retry option
- Invalid language: Default to English
- System errors: Log and return generic error

## Testing Strategy

### Unit Tests
- Test prompt builder
- Test API handler with mocked LLM service
- Test response formatting

### E2E Tests
- Test initial spell load
- Test spell regeneration
- Test multiple regenerations produce different spells
- Test with Japanese and English
- Test loading indicator appears
- Test error handling
