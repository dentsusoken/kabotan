# Error Handling

## Error Categories

1. **Validation Errors**: Invalid or missing input parameters
2. **LLM Service Errors**: Timeout, connection failure, or API errors
3. **System Errors**: Unexpected internal errors

## Error Handling Strategy

### Validation Errors
- Detect during request parsing
- Return 400 Bad Request with user-friendly message
- Log validation failure details

### LLM Service Errors
- Implement timeout (30 seconds)
- Retry logic (up to 2 retries with exponential backoff)
- Fallback to generic error message if all retries fail
- Log LLM service errors with request context

### System Errors
- Catch all unexpected errors at API layer
- Return 500 Internal Server Error with generic message
- Log full stack trace and context
- Maintain application stability (don't crash server)

## Error Response Format

All errors will be returned as HTML fragments using DaisyUI alert component:

```html
<div class="alert alert-error" role="alert">
  <svg xmlns="http://www.w3.org/2000/svg" class="stroke-current shrink-0 h-6 w-6" fill="none" viewBox="0 0 24 24">
    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
  <span>User-friendly error message</span>
  <button class="btn btn-sm" hx-post="...">Retry</button>
</div>
```

## Logging Strategy

- Log all API requests with timestamp, endpoint, and parameters (sanitized)
- Log all LLM service calls with prompt summary and response time
- Log all errors with full context and stack traces
- Use structured logging format for easy parsing
