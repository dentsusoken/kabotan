# Data Models

## Request Models

All API requests will use JSON format with the following structures:

### Monster Diagnostic Request
```json
{
  "favorite_food": "string",
  "sleep_schedule": "string",
  "hobbies": "string",
  "language": "ja|en"
}
```

### Story Generator Request
```json
{
  "name": "string",
  "style": "gothic|parody|classic",
  "elements": "string",
  "language": "ja|en"
}
```

### Character Chat Request
```json
{
  "character": "dracula|witch|jack",
  "message": "string",
  "language": "ja|en",
  "history": ["array of previous messages (optional)"]
}
```

### Trivia Bot Request
```json
{
  "message": "string",
  "language": "ja|en"
}
```

### Spell Generator Request
```json
{
  "language": "ja|en"
}
```

## Response Models

All API responses will return HTML fragments for HTMX to inject into the page:

### Success Response Structure
```
HTTP 200 OK
Content-Type: text/html; charset=utf-8

<div class="feature-response card bg-base-200">
  <!-- Feature-specific HTML content with DaisyUI classes -->
</div>
```

### Error Response Structure
```
HTTP 200 OK (HTMX handles errors via 200 with error content)
Content-Type: text/html; charset=utf-8

<div class="alert alert-error">
  <span>Error message in appropriate language</span>
</div>
```

## Internal Data Structures

### LLM Request Structure
```lisp
(defstruct llm-request
  prompt          ; string
  max-tokens      ; integer
  temperature     ; float
  timeout         ; integer (seconds))
```

### LLM Response Structure
```lisp
(defstruct llm-response
  text            ; string (generated text)
  success         ; boolean
  error-message   ; string (if success is nil)
  tokens-used     ; integer)
```

### Feature Context Structure
```lisp
(defstruct feature-context
  mode            ; keyword (:monster-diagnostic, :story-generator, etc.)
  language        ; keyword (:ja, :en)
  user-inputs     ; alist of user-provided data
  timestamp)      ; universal-time
```
