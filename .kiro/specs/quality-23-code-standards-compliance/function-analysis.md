# Function Analysis Report

## Analysis Date
2024-11-18

## Summary

This report identifies functions that violate the coding standards:
- Functions exceeding 100 lines
- Functions with nesting depth > 6 levels

## Functions Exceeding 100 Lines

### LLM Client Module (src/services/llm-client.lisp)

1. **call-openai-api** (Line 22, 110 lines)
   - Purpose: Core API client function for OpenAI-compatible endpoints
   - Issue: Long function with multiple responsibilities
   - Recommendation: Extract helper functions for request building, response parsing, and error handling

2. **call-openai-api-with-messages** (Line 132, 104 lines)
   - Purpose: Message-based API client function
   - Issue: Similar structure to call-openai-api with duplicated logic
   - Recommendation: Extract common request/response handling logic

### LLM Streaming Module (src/services/llm-streaming.lisp)

3. **call-openai-api-streaming** (Line 48, 109 lines)
   - Purpose: Streaming API client function
   - Issue: Complex streaming logic with chunk parsing
   - Recommendation: Extract chunk parsing and SSE handling into helper functions

4. **call-openai-api-with-messages-streaming** (Line 157, 114 lines)
   - Purpose: Message-based streaming API client
   - Issue: Similar to call-openai-api-streaming with duplicated logic
   - Recommendation: Extract common streaming logic

### HTML Chat Module (src/utils/html-chat.lisp)

5. **generate-character-chat-form** (Line 57, 156 lines)
   - Purpose: Generate HTML form for character chat feature
   - Issue: Large HTML template with streaming/non-streaming variants
   - Recommendation: Extract streaming and non-streaming form generation into separate helper functions

6. **generate-trivia-bot-form** (Line 241, 117 lines)
   - Purpose: Generate HTML form for trivia bot feature
   - Issue: Large HTML template with streaming/non-streaming variants
   - Recommendation: Extract streaming and non-streaming form generation into separate helper functions

### HTML Features Module (src/utils/html-features.lisp)

7. **generate-monster-diagnostic-form** (Line 100, 153 lines)
   - Purpose: Generate HTML form for monster diagnostic feature
   - Issue: Large HTML template with streaming/non-streaming variants
   - Recommendation: Extract streaming and non-streaming form generation into separate helper functions

8. **generate-story-generator-form** (Line 253, 146 lines)
   - Purpose: Generate HTML form for story generator feature
   - Issue: Large HTML template with streaming/non-streaming variants
   - Recommendation: Extract streaming and non-streaming form generation into separate helper functions

### SSE Protocol Module (src/utils/sse-protocol.lisp)

9. **create-sse-response** (Line 316, 153 lines)
   - Purpose: Create SSE response with proper headers and streaming
   - Issue: Complex streaming setup with error handling
   - Recommendation: Extract stream setup, error handling, and cleanup into helper functions

## Functions with Excessive Nesting Depth (> 6 levels)

Manual inspection of the identified long functions reveals that most have reasonable nesting depth due to Common Lisp's functional style. However, the following functions should be reviewed:

1. **create-sse-response** (src/utils/sse-protocol.lisp)
   - Contains nested handler-case, let, and lambda forms
   - Recommendation: Extract inner logic into helper functions

2. **call-openai-api-streaming** (src/services/llm-streaming.lisp)
   - Contains nested loop with conditional logic for chunk parsing
   - Recommendation: Extract chunk parsing logic

3. **generate-character-chat-form** (src/utils/html-chat.lisp)
   - Contains nested conditionals for streaming/non-streaming variants
   - Recommendation: Split into separate functions

## Refactoring Strategy

### Phase 1: LLM Client Functions
- Extract common request building logic
- Extract common response parsing logic
- Extract error handling logic
- Reduce duplication between regular and message-based functions

### Phase 2: LLM Streaming Functions
- Extract chunk parsing logic
- Extract SSE event handling
- Reduce duplication between regular and message-based functions

### Phase 3: HTML Form Generation Functions
- Extract streaming form generation into helper functions
- Extract non-streaming form generation into helper functions
- Create common form building utilities

### Phase 4: SSE Response Function
- Extract stream setup logic
- Extract error handling logic
- Extract cleanup logic

## Testing Requirements

After each refactoring:
1. Run complete test suite to verify no regressions
2. Verify all functions pass existing tests
3. Ensure refactored functions maintain original interfaces
4. Test both streaming and non-streaming modes

## Success Criteria

- All functions under 100 lines
- All functions with nesting depth ≤ 6 levels
- All tests passing
- No functionality changes
- Improved code maintainability
