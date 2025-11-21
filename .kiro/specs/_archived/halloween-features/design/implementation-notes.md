# Implementation Notes

## Existing Code Integration

The design builds upon the existing Kabotan codebase:
- LLM service reads configuration directly from environment variables
- Extend existing Ningle app structure
- Maintain existing Lack middleware stack
- Keep existing health check endpoint
- Replace placeholder `index.html` with full Halloween UI

## Environment Configuration

The application will read LLM configuration from environment variables:

### Required Environment Variables
- `OPENAI_API_KEY`: API key for OpenAI-compatible service (default: "dummy")
- `OPENAI_MODEL`: Model name to use (default: "gemma3-12b")
- `OPENAI_HOST`: LLM service endpoint URL (default: "http://localhost:8080/v1/chat/completions")

### Configuration Loading
Configuration is now handled directly in the LLM service layer:
```lisp
;; In llm-service.lisp, environment variables are read on each API call:
(let* ((api-key (or (uiop:getenv "OPENAI_API_KEY") 
                   (uiop:getenv "OPENAI_KEY") 
                   "dummy"))
       (model (or (uiop:getenv "OPENAI_MODEL") "gemma3-12b"))
       (host (or (uiop:getenv "OPENAI_HOST") 
                "http://localhost:8080/v1/chat/completions")))
  ...)
```

This allows deployment flexibility without code changes.

## File Organization

New files to be created:
```
src/
  api/
    halloween-api.lisp
  services/
    llm-service.lisp
    prompt-builder.lisp
    language-handler.lisp
  utils/
    validation.lisp
    error-handling.lisp
    response-formatting.lisp
public/
  index.html (replace existing with TailwindCSS/DaisyUI)
  custom-styles.css (minimal custom styles for Halloween theme overrides)
tests/
  api/
    halloween-api-test.lisp
  services/
    llm-service-test.lisp
    prompt-builder-test.lisp
    language-handler-test.lisp
  utils/
    validation-test.lisp
    response-formatting-test.lisp
  integration/
    llm-integration-test.lisp
  e2e/
    monster-diagnostic.spec.js
    story-generator.spec.js
    character-chat.spec.js
    trivia-bot.spec.js
    spell-generator.spec.js
    language-switching.spec.js
```

## ASDF System Updates

Update `kabotan.asd` to include new source files in correct load order:
1. Package definition
2. Utilities
3. Services
4. API layer
5. Main application

## Performance Considerations

- LLM calls are the primary bottleneck (expect 5-15 second response times)
- Use HTMX loading indicators to manage user expectations
- Spell generator creates new content on each request (no date-based caching)
- Browser session can preserve current spell until user requests regeneration
- Implement request queuing if concurrent load is high
- Monitor LLM service response times and adjust timeouts accordingly

## Security Considerations

- Sanitize all user inputs before sending to LLM
- Validate all request parameters
- Use HTTPS in production
- Don't expose internal error details to users
- Rate limit API endpoints to prevent abuse
- Log security-relevant events

## Localization Strategy

- Store all UI text in `language-handler.lisp` data structure
- LLM prompts include language instructions
- Date formatting respects locale
- Error messages translated to user's selected language
- Default language detected from browser Accept-Language header
