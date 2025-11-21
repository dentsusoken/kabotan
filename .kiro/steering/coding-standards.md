# Coding Standards

## Common Lisp

### Code Structure
- **S-expression nesting depth**: Keep nesting depth at 6 levels or less
  - If nesting exceeds 6 levels, consider refactoring into helper functions
  - Deep nesting often indicates complex logic that should be broken down
  
- **Function length**: Functions should not exceed 100 lines
  - If a function exceeds 100 lines, evaluate whether it can be split into smaller, focused functions
  - Each function should have a single, well-defined responsibility

- **File length**: Keep files at approximately 500 lines or less
  - If a file exceeds 500 lines, consider splitting it into multiple files
  - Group related functionality into separate modules
  - Use clear file naming to indicate the purpose of each module

### Examples

#### Good: Shallow nesting (depth ≤ 6)
```lisp
(defun process-request (request)
  (let ((data (parse-request request)))
    (when (valid-p data)
      (let ((result (process-data data)))
        (format-response result)))))
```

#### Bad: Deep nesting (depth > 6)
```lisp
(defun complex-function (input)
  (let ((a (foo input)))
    (when a
      (let ((b (bar a)))
        (if b
          (let ((c (baz b)))
            (when c
              (let ((d (qux c)))
                (if d
                  (let ((e (quux d)))
                    ;; Too deep!
                    )))))))))
```

## HTML & JavaScript

### File Length
- **JavaScript file length**: Keep JavaScript files at 500 lines or less
  - If a file exceeds 500 lines, split it into multiple focused modules
  - Group related functionality into separate files
  - Use clear file naming to indicate the purpose of each module
  - Examples of good module separation:
    - `feature-content-templates.js` - HTML template definitions
    - `feature-streaming-handlers.js` - Streaming functionality
    - `feature-ui-handlers.js` - UI event handlers
    - `feature-manager.js` - Core feature management

### Function Naming and Uniqueness
- **No duplicate function names**: Each function must have a unique name across all JavaScript files
  - Duplicate function names can cause unexpected behavior and are difficult to debug
  - Use descriptive, specific names that indicate the function's purpose and scope
  - If similar functionality is needed in multiple files, extract it to a shared utility module
  - Use prefixes or namespaces to avoid naming conflicts (e.g., `chatEscapeHtml`, `formEscapeHtml`)
  - Before adding a new function, search the codebase to ensure the name is not already used
  - Examples of good naming:
    - ❌ Bad: `escapeHtml()` in multiple files
    - ✅ Good: `escapeHtml()` in one shared utility file, imported by others
    - ✅ Good: `chatEscapeHtml()` and `formEscapeHtml()` if they have different implementations

### Separation of Concerns
- **JavaScript in HTML is strictly prohibited**: Never write JavaScript code directly in HTML files
  - All JavaScript must be in separate `.js` files in the `public/js/` directory
  - Use external script references: `<script src="/static/js/filename.js?v=VERSION"></script>`
  - This includes:
    - Inline `<script>` tags with code
    - Inline event handlers (e.g., `onclick="..."`)
    - JavaScript in `style` attributes
  - Use HTMX attributes (`hx-get`, `hx-post`, etc.) for interactions instead of JavaScript event handlers
  - Use event listeners in JS files for functionality not provided by HTMX
  - Always include cache-busting version parameter (`?v=YYYYMMDD`)

### HTMX-Driven Architecture
- **Server generates HTML, not JSON**: All API endpoints should return HTML fragments
  - Use `html-templates.lisp` functions to generate HTML on the server
  - Return HTML with proper `Content-Type: text/html; charset=utf-8` headers
  - Include HTMX attributes in generated HTML for subsequent interactions
  - Escape all user-generated content using `escape-html` function
- **Declarative interactions**: Use HTMX attributes for dynamic behavior
  - `hx-get`, `hx-post` for requests
  - `hx-target` to specify where to swap content
  - `hx-swap` to control swap strategy
  - `sse-connect`, `sse-swap` for streaming responses
  - `hx-ext` to enable extensions (sse, response-targets)
- **Minimal client-side JavaScript**: Only implement functionality not provided by HTMX
  - Language preference management
  - Page initialization
  - Custom streaming handlers if needed

### Examples

#### Good: Separated JavaScript
```html
<!-- index.html -->
<button id="submit-btn">Submit</button>
<script src="/static/js/form-handler.js?v=20241115"></script>
```

```javascript
// public/js/form-handler.js
document.getElementById('submit-btn').addEventListener('click', function() {
  // Handle click
});
```

#### Bad: Inline JavaScript
```html
<!-- DON'T DO THIS -->
<button onclick="handleClick()">Submit</button>
<script>
  function handleClick() {
    // Inline script
  }
</script>
```

### Rationale
- **Maintainability**: Separating JavaScript from HTML makes code easier to maintain and debug
- **Reusability**: External JavaScript files can be cached by browsers and reused across pages
- **Security**: Reduces risk of XSS attacks and makes Content Security Policy implementation easier
- **Testing**: External JavaScript files are easier to unit test
- **Code Review**: Changes to JavaScript logic are clearly visible in version control

## General Principles
- Keep code modular and maintainable
- Prioritize readability over cleverness
- Follow the single responsibility principle
- Write self-documenting code with clear naming
