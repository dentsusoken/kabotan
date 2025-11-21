# Design Document

## Overview

This document describes the design for implementing Halloween-themed interactive features in the Kabotan web application. The system will leverage the existing Common Lisp backend with Ningle framework and integrate HTMX for dynamic frontend interactions. The architecture follows a modular approach where each Halloween feature is implemented as a separate API endpoint with shared LLM integration utilities.

The application will provide five distinct feature modes (Monster Diagnostic, Story Generator, Character Chat, Trivia Bot, Spell Generator) accessible through a unified interface with language switching capabilities. All features will utilize an OpenAI-compatible LLM service integration via HTTP API.

## Design Documents

This design is split into multiple documents for easier reference:

- **[Architecture](design/architecture.md)**: High-level architecture, technology stack, and design principles
- **[Components](design/components.md)**: Frontend and backend component specifications
- **[Data Models](design/data-models.md)**: Request/response formats and internal data structures
- **[Error Handling](design/error-handling.md)**: Error categories, handling strategies, and logging
- **[Testing Strategy](design/testing-strategy.md)**: Unit, integration, and E2E testing approach
- **[Implementation Notes](design/implementation-notes.md)**: Integration with existing code, file organization, and configuration

## Quick Reference

### Key Technologies
- Backend: Common Lisp (Ningle, Clack, Lack, Dexador, cl-json)
- Frontend: HTMX, TailwindCSS, DaisyUI (dark mode)
- LLM: OpenAI-compatible API via HTTP
- Testing: FiveAM (backend), Playwright (E2E)

### API Endpoints
- POST `/api/monster-diagnostic` - Monster personality diagnosis
- POST `/api/story-generator` - Halloween story generation
- POST `/api/character-chat` - Character-based chat
- POST `/api/trivia-bot` - Conversation with trivia
- POST `/api/spell-generator` - Generate magical phrase on demand

### Environment Variables
- `OPENAI_API_KEY` - API key for LLM service
- `OPENAI_MODEL` - Model name to use
- `OPENAI_HOST` - LLM service endpoint URL
