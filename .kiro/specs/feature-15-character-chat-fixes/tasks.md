# Implementation Plan

- [x] 1. Improve radio button group styling
  - Modify `generate-form-radio-group` in `src/utils/html-forms.lisp`
  - Change outer container spacing from `gap-2` to `gap-8` for better separation between options
  - Change inner label spacing from `gap-2` to `gap-3` for better radio-to-label spacing
  - Add `px-3` padding to labels for larger clickable area
  - Add `min-w-fit` class to prevent label text wrapping
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 1.1 Write unit test for radio button HTML structure
  - Test that generated HTML includes `gap-8` in outer container
  - Test that generated HTML includes `gap-3` in label elements
  - Test that generated HTML includes `px-3` in label elements
  - Test that generated HTML includes `min-w-fit` in label elements
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 2. Move character selector inside form (streaming version)
  - Modify `generate-character-chat-streaming-form` in `src/utils/html-chat.lisp`
  - Move character-selector parameter from outside form to inside form
  - Place character-selector after hidden language input
  - Place character-selector before message input
  - Maintain proper indentation and structure
  - _Requirements: 1.1, 1.2_

- [x] 3. ~~Move character selector inside form (non-streaming version)~~ (Skipped - non-streaming not used)
  - ~~Modify `generate-character-chat-non-streaming-form` in `src/utils/html-chat.lisp`~~
  - ~~Apply same structural changes as streaming version~~
  - ~~Ensure consistency between streaming and non-streaming forms~~
  - _Requirements: 1.1, 1.3_ (Non-streaming removed from requirements)

- [x] 3.1 Write unit test for character selector placement
  - Test that character selector appears inside form element
  - Test that character selector appears after language input
  - Test that character selector appears before message input
  - Test streaming version only (non-streaming not used)
  - _Requirements: 1.1, 1.2_

- [x] 4. Checkpoint - Verify implementation
  - Ensure all tests pass, ask the user if questions arise

- [x] 5. E2E test for character selection (streaming)
  - Test selecting Dracula and verify response
  - Test selecting Witch and verify response
  - Test selecting Jack-o'-lantern and verify response
  - Verify character parameter is transmitted correctly
  - _Requirements: 1.1, 1.2, 1.4_

- [x] 6. E2E test for radio button UI interaction
  - Verify radio buttons are visually spaced apart
  - Verify clicking on label text selects the radio button
  - Verify adequate clickable area around each option
  - _Requirements: 2.1, 2.2, 2.3_
