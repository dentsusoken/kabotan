# Design Document

## Overview

This design addresses two critical issues in the character chat feature:

1. **Missing Character Parameter**: The character selection is not being transmitted in form submissions because the radio button group is placed outside the form element
2. **Poor Radio Button UI**: The radio buttons have insufficient spacing and misalignment, making them difficult to use

The solution involves restructuring the HTML generation to include the character selector inside the form and improving the radio button group styling.

## Architecture

### Current Architecture Issues

**Problem 1: Character Selector Outside Form**
```
<div id="character-chat-content">
  <div><!-- Character selector radio group --></div>
  <div><!-- Chat history --></div>
  <form>
    <!-- Message input -->
    <!-- Submit button -->
  </form>
</div>
```

The character selector is outside the `<form>` element, so its value is not included in form submissions.

**Problem 2: Radio Button Styling**
```html
<label class="cursor-pointer flex items-center gap-2">
  <input type="radio" ... />
  <span>Label</span>
</label>
```

Current styling uses `gap-2` (0.5rem) which is too small, and lacks proper padding for clickable area.

### Proposed Architecture

**Solution 1: Move Character Selector Inside Form**
```
<div id="character-chat-content">
  <div><!-- Chat history --></div>
  <form>
    <!-- Character selector radio group -->
    <!-- Message input -->
    <!-- Submit button -->
  </form>
</div>
```

**Solution 2: Improve Radio Button Styling**
```html
<label class="cursor-pointer flex items-center gap-3 px-3 min-w-fit">
  <input type="radio" ... />
  <span>Label</span>
</label>
```

Changes:
- `gap-3` (0.75rem) instead of `gap-2` for better spacing between radio and label
- `px-3` (0.75rem horizontal padding) for larger clickable area
- `min-w-fit` to prevent label wrapping
- Outer container uses `gap-8` (2rem) for spacing between options

## Components and Interfaces

### Modified Components

#### 1. `generate-form-radio-group` (src/utils/html-forms.lisp)

**Current Implementation:**
```lisp
(defun generate-form-radio-group (&key name label options required selected class id)
  ;; Generates radio group with gap-2 spacing
  )
```

**Modified Implementation:**
```lisp
(defun generate-form-radio-group (&key name label options required selected class id)
  ;; Generates radio group with:
  ;; - gap-8 between options (outer container)
  ;; - gap-3 between radio and label (inner label)
  ;; - px-3 padding on label for clickable area
  ;; - min-w-fit to prevent wrapping
  )
```

#### 2. `generate-character-chat-streaming-form` (src/utils/html-chat.lisp)

**Current Structure:**
```lisp
(format nil "~
  <p>~A</p>
  <div id=\"character-chat-content\">
    ~A  <!-- character-selector OUTSIDE form -->
    <div id=\"chat-history-container\">~A</div>
    <form>
      <input type=\"hidden\" name=\"language\" value=\"~A\" />
      ~A  <!-- message-input -->
      <div>~A ~A</div>  <!-- submit-button, loading-indicator -->
    </form>
  </div>"
  description character-selector history-html language
  message-input submit-button loading-indicator)
```

**Modified Structure:**
```lisp
(format nil "~
  <p>~A</p>
  <div id=\"character-chat-content\">
    <div id=\"chat-history-container\">~A</div>
    <form>
      <input type=\"hidden\" name=\"language\" value=\"~A\" />
      ~A  <!-- character-selector INSIDE form -->
      ~A  <!-- message-input -->
      <div>~A ~A</div>  <!-- submit-button, loading-indicator -->
    </form>
  </div>"
  description history-html language
  character-selector message-input submit-button loading-indicator)
```

### No Changes Required

- `handle-character-chat-streaming`: Already handles character parameter correctly
- `handle-character-chat-request`: Already handles character parameter correctly
- Character normalization logic: Already works correctly

## Data Models

No data model changes required. The existing parameter extraction and validation logic already handles the character parameter correctly.

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Character parameter inclusion in form submission

*For any* character selection and message input, when the form is submitted, the HTTP request should include the character parameter with the selected value.

**Validates: Requirements 1.1, 1.2**

### Property 2: Radio button spacing consistency

*For any* radio button group with multiple options, the vertical spacing between options should be consistent and adequate for easy selection (minimum 2rem between options).

**Validates: Requirements 2.1, 2.3**

### Property 3: Radio button alignment

*For any* radio button option, the radio input and its label should be horizontally aligned at the center.

**Validates: Requirements 2.2**

## Error Handling

No new error handling required. Existing validation and error handling for character parameter is sufficient:

- Character normalization handles variations (jack-o-lantern → jack)
- Default fallback to "dracula" for invalid characters
- Existing validation for message parameter

## Testing Strategy

### Unit Tests

**Test 1: Radio button HTML structure**
- Verify `generate-form-radio-group` produces correct HTML with improved spacing
- Check for `gap-8` in outer container
- Check for `gap-3` and `px-3` in label elements
- Verify `min-w-fit` class is present

**Test 2: Character selector inside form**
- Verify `generate-character-chat-streaming-form` places character selector inside form
- Check that character selector appears after hidden language input
- Check that character selector appears before message input

### E2E Tests

**Test 3: Character selection transmission (streaming)**
- Select each character (Dracula, Witch, Jack-o'-lantern)
- Submit a message
- Verify backend receives correct character parameter
- Verify response is from the selected character

**Test 4: Radio button UI interaction**
- Verify radio buttons are visually spaced apart
- Verify clicking on label text selects the radio button
- Verify adequate clickable area around each option

### Property-Based Tests

Not applicable for this feature - the changes are primarily structural and UI-focused rather than algorithmic.

### Testing Framework

- Backend unit tests: FiveAM
- E2E tests: Playwright
- Manual testing: Visual inspection of radio button spacing and alignment
