# Components and Interfaces

## Frontend Components

### 1. Main Application Shell (`public/index.html`)

Provides the overall page structure with:
- TailwindCSS and DaisyUI integration via CDN
- Dark mode theme with Halloween color scheme (orange, purple, dark backgrounds)
- Language selector (Japanese/English toggle) using DaisyUI toggle component
- Feature mode navigation menu using DaisyUI tabs or drawer component
- Dynamic content container for feature-specific UI with DaisyUI cards
- Loading indicators using DaisyUI loading spinner
- Error message display area using DaisyUI alert component
- Responsive layout with mobile-first design

### 2. Feature Mode Components (HTMX partials)

Each feature mode has its own HTML partial returned by the backend:

**Monster Diagnostic Component**
- DaisyUI form with input fields: favorite food, sleep schedule, hobbies
- DaisyUI button (primary, Halloween-themed) triggering HTMX POST to `/api/monster-diagnostic`
- Result display area using DaisyUI card with monster type and description
- Dark theme with orange/purple accents

**Story Generator Component**
- DaisyUI form with input fields: user name, story elements
- DaisyUI select dropdown for style: Gothic, Parody, Classic Ghost Story
- DaisyUI button triggering HTMX POST to `/api/story-generator`
- Story display area using DaisyUI card with formatted narrative
- Typography optimized for reading stories in dark mode

**Character Chat Component**
- DaisyUI radio buttons or tabs for character selector: Dracula, Witch, Jack-o'-Lantern
- Chat message input using DaisyUI textarea
- Chat history display using DaisyUI chat bubbles (alternating user/character)
- Submit button using DaisyUI button component
- Character avatars with Halloween icons

**Trivia Bot Component**
- Conversation input field using DaisyUI textarea
- Submit button using DaisyUI button component
- Response display with DaisyUI badge or alert for highlighted trivia facts
- Dark theme with distinct styling for trivia highlights

**Spell Generator Component**
- Automatic load on component mount via HTMX POST to `/api/spell-generator`
- Regenerate button using DaisyUI button component triggering new spell generation
- Spell display area using DaisyUI card with mystical styling
- Phrase displayed in larger, decorative text
- Explanation in smaller text below
- Loading state shown during regeneration

## Backend Components

### 1. API Layer (`src/api/`)

**File: `halloween-api.lisp`**

Defines all Halloween feature endpoints:

```lisp
;; Monster Diagnostic Endpoint
;; POST /api/monster-diagnostic
;; Request: { "favorite_food": "...", "sleep_schedule": "...", "hobbies": "...", "language": "ja|en" }
;; Response: HTML fragment with monster diagnosis

;; Story Generator Endpoint
;; POST /api/story-generator
;; Request: { "name": "...", "style": "gothic|parody|classic", "elements": "...", "language": "ja|en" }
;; Response: HTML fragment with generated story

;; Character Chat Endpoint
;; POST /api/character-chat
;; Request: { "character": "dracula|witch|jack", "message": "...", "language": "ja|en" }
;; Response: HTML fragment with character response

;; Trivia Bot Endpoint
;; POST /api/trivia-bot
;; Request: { "message": "...", "language": "ja|en" }
;; Response: HTML fragment with response and trivia

;; Spell Generator Endpoint
;; POST /api/spell-generator
;; Request: { "language": "ja|en" }
;; Response: HTML fragment with generated spell
```

Each endpoint will:
1. Parse and validate incoming request parameters
2. Call appropriate service layer function
3. Handle errors and timeouts
4. Format response as HTML fragment for HTMX
5. Set appropriate HTTP headers

### 2. Service Layer (`src/services/`)

**File: `llm-service.lisp`**

Provides core LLM interaction functionality:

```lisp
(defun call-llm (prompt &key (max-tokens 1000) (temperature 0.7))
  "Calls the LLM service with the given prompt and returns the response text.
   Handles timeouts and errors, returning nil on failure.")

(defun call-llm-with-retry (prompt &key (max-retries 2) (timeout 30))
  "Calls LLM with retry logic for resilience.")
```

**File: `prompt-builder.lisp`**

Constructs prompts for each feature mode:

```lisp
(defun build-monster-diagnostic-prompt (inputs language)
  "Builds a prompt for monster personality diagnosis based on user inputs.")

(defun build-story-generator-prompt (name style elements language)
  "Builds a prompt for story generation with specified style.")

(defun build-character-chat-prompt (character message language)
  "Builds a prompt for character-based chat responses.")

(defun build-trivia-bot-prompt (message language)
  "Builds a prompt that includes Halloween trivia in responses.")

(defun build-spell-generator-prompt (language)
  "Builds a prompt for generating a magical phrase or spell.")
```

Each prompt builder will:
- Include system instructions for the LLM
- Incorporate Halloween theme context
- Specify output format requirements
- Handle language-specific instructions

**File: `language-handler.lisp`**

Manages language-specific logic:

```lisp
(defun get-ui-text (key language)
  "Returns UI text for the given key in the specified language.")

(defun detect-browser-language (accept-language-header)
  "Detects preferred language from HTTP Accept-Language header.")

(defparameter *ui-texts*
  '((:ja . ((:monster-diagnostic-title . "モンスター性格診断")
            (:story-generator-title . "ハロウィーン物語ジェネレーター")
            ...))
    (:en . ((:monster-diagnostic-title . "Monster Personality Diagnostic")
            (:story-generator-title . "Halloween Story Generator")
            ...))))
```

### 3. Utilities (`src/utils/`)

**File: `validation.lisp`**

Input validation functions:

```lisp
(defun validate-language (lang)
  "Validates language parameter is 'ja' or 'en'.")

(defun validate-non-empty-string (str field-name)
  "Validates that a string field is non-empty.")

(defun validate-story-style (style)
  "Validates story style is one of: gothic, parody, classic.")

(defun validate-character (character)
  "Validates character is one of: dracula, witch, jack.")

(defun sanitize-input (input)
  "Sanitizes user input to prevent injection attacks.")
```

**File: `error-handling.lisp`**

Error handling utilities:

```lisp
(defun handle-api-error (condition)
  "Handles API errors and returns appropriate HTTP response.")

(defun format-error-response (message language)
  "Formats error message as HTML fragment for HTMX.")

(defun log-error (context error-details)
  "Logs error information for debugging.")
```

**File: `response-formatting.lisp`**

Response formatting utilities:

```lisp
(defun format-html-response (content)
  "Wraps content in appropriate HTML structure for HTMX.")

(defun format-monster-diagnosis (monster-type description language)
  "Formats monster diagnosis as styled HTML.")

(defun format-story (title story language)
  "Formats generated story as styled HTML.")

(defun format-chat-message (character message language)
  "Formats character chat message with character indicator.")

(defun format-trivia-response (response trivia-facts language)
  "Formats response with highlighted trivia facts.")

(defun format-spell (spell explanation language)
  "Formats generated spell with explanation.")
```
