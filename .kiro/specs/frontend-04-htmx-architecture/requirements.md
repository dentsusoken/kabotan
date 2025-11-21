# HTMX Frontend Integration - Requirements

## Overview
This specification defines the HTMX-based frontend architecture for Kabotan, providing dynamic user interactions without heavy JavaScript frameworks. The frontend uses HTMX for server communication, DaisyUI for styling, and modular JavaScript for feature management.

## User Stories

### US-1: Dynamic Feature Switching
**As a** user  
**I want to** switch between different Halloween features using tabs  
**So that** I can access multiple features without page reloads

**Acceptance Criteria:**
- Tab navigation switches content dynamically
- Active tab is visually highlighted
- Content loads instantly without full page refresh
- URL does not change when switching tabs
- Previous feature state is cleared when switching

### US-2: Form Submission with HTMX
**As a** user  
**I want to** submit forms and see results without page reloads  
**So that** I have a smooth, app-like experience

**Acceptance Criteria:**
- Forms submit via AJAX using HTMX
- Loading indicators appear during submission
- Results appear in designated target areas
- Form inputs remain accessible during submission
- Error messages display inline when submission fails

### US-3: Streaming Response Display
**As a** user  
**I want to** see LLM responses appear progressively  
**So that** I know the system is working and can read content as it arrives

**Acceptance Criteria:**
- Streaming indicator shows when response is generating
- Content appears progressively in real-time
- Stop button allows canceling ongoing streams
- Partial content is preserved if stream is interrupted
- Fallback to non-streaming mode if browser doesn't support SSE

### US-4: Conversation History Management
**As a** user  
**I want to** have contextual conversations with chat features  
**So that** the system remembers previous messages in the conversation

**Acceptance Criteria:**
- Chat history is maintained in sessionStorage
- History is sent with each new message
- History is limited to last 10 messages
- Clear button removes all history
- History persists across tab switches but not browser sessions

### US-5: Multi-language Support
**As a** user  
**I want to** switch between Japanese and English  
**So that** I can use the application in my preferred language

**Acceptance Criteria:**
- Language toggle switches all UI text
- Language preference is saved to localStorage
- Browser language is detected on first visit
- All forms include language parameter
- Language change reloads dynamic content (e.g., spell generator)

### US-6: Error Handling and Recovery
**As a** user  
**I want to** see clear error messages and recovery options  
**So that** I can understand what went wrong and try again

**Acceptance Criteria:**
- HTTP errors display user-friendly messages
- Timeout errors are handled gracefully
- Streaming errors show retry and fallback options
- Partial content is preserved on streaming errors
- Error messages auto-hide after 5 seconds

### US-7: Mobile Responsive Design
**As a** mobile user  
**I want to** use all features comfortably on my phone  
**So that** I can access Kabotan from any device

**Acceptance Criteria:**
- Tabs scroll horizontally on mobile
- Touch targets are at least 48x48 pixels
- Input fields prevent zoom on iOS (16px font minimum)
- Chat containers adjust height for mobile viewports
- Buttons stack vertically on small screens

### US-8: Browser Compatibility Detection
**As a** user with an older browser  
**I want to** be informed if streaming is not supported  
**So that** I understand why the experience differs

**Acceptance Criteria:**
- Browser compatibility is detected on page load
- Compatibility message shows once per session
- Message explains browser limitations
- Recommendation for modern browsers is provided
- Application falls back to non-streaming mode automatically

## Technical Requirements

### TR-1: HTMX Integration
- Use HTMX 1.9.10 or later via CDN
- Configure global timeout to 360 seconds
- Use `hx-post` for form submissions
- Use `hx-target` and `hx-swap` for response placement
- Use `hx-indicator` for loading states

### TR-2: JavaScript Module Organization
- Separate concerns into distinct modules:
  - `feature-manager.js` - Feature switching and content loading
  - `language-manager.js` - Language switching and translations
  - `streaming-manager.js` - SSE connection management
  - `chat-manager.js` - Conversation history management
  - `error-handler.js` - Error display and handling
- No inline JavaScript in HTML files
- All modules loaded via external script tags

### TR-3: CSS Framework Integration
- Use TailwindCSS 3.x via CDN for utility classes
- Use DaisyUI 4.4.19 for component styling
- Custom Halloween theme with purple/orange gradient
- Responsive breakpoints: mobile (<768px), tablet (768-1024px), desktop (>1024px)

### TR-4: State Management
- Use sessionStorage for conversation history
- Use localStorage for language preference
- Use window object for active streaming manager references
- Clear state when switching features

### TR-5: Performance Optimization
- Throttle DOM updates to 10 per second (100ms interval)
- Use requestAnimationFrame for smooth updates
- Implement string builder pattern for efficient concatenation
- Batch multiple chunks into single DOM update

### TR-6: Accessibility
- Semantic HTML structure
- ARIA labels for interactive elements
- Keyboard navigation support
- Screen reader friendly error messages
- Focus management for dynamic content

## Dependencies

### Depends On:
- **01-core-infrastructure** - Backend API endpoints
- **02-streaming-support** - SSE streaming endpoints
- **03-llm-enhancements** - Conversation history and system prompts

### Required By:
- **10-character-chat** - Chat UI implementation
- **11-monster-diagnostic** - Diagnostic form UI
- **12-story-generator** - Story form UI
- **13-spell-generator** - Spell display UI
- **14-trivia-bot** - Trivia chat UI
- **21-e2e-testing** - E2E test infrastructure

## Non-Functional Requirements

### NFR-1: Performance
- Initial page load < 2 seconds
- Feature switch < 100ms
- Form submission response < 500ms (excluding LLM processing)
- Streaming updates appear within 100ms of receiving data

### NFR-2: Browser Support
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+
- Graceful degradation for older browsers

### NFR-3: Mobile Performance
- Touch response < 100ms
- Smooth scrolling at 60fps
- No layout shifts during content loading
- Efficient memory usage for long conversations

### NFR-4: Code Quality
- No JavaScript in HTML files
- ESLint compliant code
- Comprehensive error handling
- Detailed console logging for debugging

## Out of Scope

- Single Page Application (SPA) routing
- Progressive Web App (PWA) features
- Offline functionality
- Service Worker implementation
- WebSocket connections (using SSE instead)
- Complex state management libraries (Redux, MobX, etc.)
