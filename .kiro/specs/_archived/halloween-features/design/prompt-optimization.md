# LLM Prompt Optimization

## Overview

This document describes the optimizations made to LLM prompts and parameters for each Halloween feature to improve response quality, consistency, and user experience.

## Optimization Strategy

### 1. Prompt Structure Improvements

All prompts were enhanced with:
- **Clearer role definitions**: More specific character descriptions and expertise areas
- **Explicit formatting instructions**: Exact format requirements with examples
- **Detailed guidelines**: Specific requirements for tone, length, and content
- **Better context**: More background information for the LLM to understand the task

### 2. Parameter Tuning

Each feature was assigned optimal `max-tokens` and `temperature` values based on:
- **Response length requirements**: Shorter for chat, longer for stories
- **Creativity needs**: Higher temperature for creative tasks, lower for factual
- **Consistency requirements**: Lower temperature for reproducible outputs

## Feature-Specific Optimizations

### Monster Diagnostic

**Prompt Improvements:**
- Added "expert analyst" framing with psychology knowledge
- Explicit instructions to reference each user trait in analysis
- Clearer format requirements with section labels
- Request for 3-4 sentence analysis (previously 2-3)
- Emphasis on making clear connections between traits and monster

**Parameters:**
- `max-tokens`: 600 (reduced from 1000 for focused responses)
- `temperature`: 0.85 (increased from 0.8 for more creative personality matching)

**Rationale:** Monster diagnostic needs creative personality analysis but should stay focused. 600 tokens is sufficient for the structured format (type + analysis + fun fact).

### Story Generator

**Prompt Improvements:**
- Detailed style-specific instructions for each genre (Gothic, Parody, Classic)
- Explicit story structure requirements (setup, conflict, resolution)
- Emphasis on sensory details and atmosphere
- Increased length requirement to 4-6 paragraphs (from 3-5)
- Instructions for organic theme integration
- Specific ending requirements based on style

**Parameters:**
- `max-tokens`: 1200 (increased from 1000 for longer stories)
- `temperature`: 0.9 (maintained for maximum creativity)

**Rationale:** Stories need the most creative freedom and length. Higher token count allows for richer narratives with better pacing.

### Character Chat

**Prompt Improvements:**
- Expanded character trait descriptions with specific examples
- Added speech pattern guidelines for each character
- Bullet-pointed personality traits for clarity
- Explicit task requirements (stay in character, show personality, engage naturally)
- Character-specific mannerisms and references

**Parameters:**
- `max-tokens`: 400 (increased from 300 for richer responses)
- `temperature`: 0.85 (increased from 0.8 for more personality variation)

**Rationale:** Character responses need personality and variety. 400 tokens allows for 2-4 well-developed sentences with character details.

### Trivia Bot

**Prompt Improvements:**
- Framed as "Halloween historian and trivia expert"
- Explicit requirements for trivia quality (lesser-known, specific details)
- Clear structure: response + trivia with emoji marker
- Emphasis on making trivia relevant to conversation
- Examples of good trivia (dates, places, cultures, practices)

**Parameters:**
- `max-tokens`: 600 (increased from 500 for detailed trivia)
- `temperature`: 0.75 (increased from 0.7 for more interesting facts)

**Rationale:** Trivia needs to be factual but engaging. Moderate temperature balances accuracy with variety. 600 tokens allows for conversational response plus detailed trivia.

### Daily Spell

**Prompt Improvements:**
- Framed as "ancient spell weaver from mystical grimoire"
- Instructions to use date as inspiration for theme
- Emphasis on making spells sound chantable and mystical
- Explicit consistency requirement (same date = same spell)
- Detailed format requirements with section labels
- Request for 2-3 sentence meaning explanation

**Parameters:**
- `max-tokens`: 500 (reduced from 1000 for focused spell + meaning)
- `temperature`: 0.7 (maintained for consistency)

**Rationale:** Daily spells need consistency for the same date. Lower temperature helps reproducibility. 500 tokens is sufficient for spell phrase and explanation.

## Results and Benefits

### Expected Improvements

1. **Better Quality Responses**
   - More detailed and insightful monster analyses
   - Richer, more atmospheric stories
   - Stronger character personalities in chat
   - More interesting and specific trivia facts
   - More mystical and memorable daily spells

2. **Improved Consistency**
   - Clearer format adherence across all features
   - More predictable response lengths
   - Better alignment with user expectations

3. **Enhanced User Experience**
   - More engaging and entertaining content
   - Better integration of user inputs
   - Stronger thematic consistency
   - More memorable interactions

### Testing Recommendations

To validate these optimizations:

1. **Test with varied inputs**: Try different personality traits, story themes, character messages
2. **Check format compliance**: Verify responses follow the specified format
3. **Evaluate quality**: Assess creativity, relevance, and engagement
4. **Test edge cases**: Very short inputs, unusual requests, different languages
5. **Monitor token usage**: Ensure responses fit within token limits
6. **Compare before/after**: If possible, compare with previous prompt versions

## Future Optimization Opportunities

1. **A/B Testing**: Compare different prompt variations for the same feature
2. **User Feedback**: Collect ratings on response quality
3. **Token Efficiency**: Further tune max-tokens based on actual usage patterns
4. **Prompt Templates**: Create reusable prompt components for consistency
5. **Language-Specific Tuning**: Optimize prompts differently for Japanese vs English
6. **Context Management**: Implement conversation history for multi-turn interactions

## Technical Notes

### Temperature Guidelines

- **0.7-0.75**: Factual content with some variety (trivia, daily spell)
- **0.8-0.85**: Creative content with personality (monster diagnostic, character chat)
- **0.9+**: Maximum creativity (story generation)

### Token Allocation

- **300-400**: Short conversational responses (character chat)
- **500-600**: Medium structured responses (monster diagnostic, trivia, daily spell)
- **1000-1200**: Long narrative content (story generation)

### Prompt Structure Best Practices

1. Start with clear role definition
2. Provide specific task instructions
3. Include format requirements with examples
4. Add constraints and guidelines
5. End with language instruction
6. Use explicit section markers for structured output
