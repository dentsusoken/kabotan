# Requirements Document

## Introduction

This document outlines the requirements for refactoring the Kabotan frontend architecture to align with HTMX's hypermedia-driven design principles. The current implementation relies heavily on client-side JavaScript for rendering and state management, which contradicts HTMX's philosophy of server-driven HTML responses. This refactoring will move rendering logic to the server, utilize HTMX's SSE extension for streaming, and significantly reduce client-side JavaScript complexity.

## Glossary

- **HTMX**: A JavaScript library that enables modern browser features through HTML attributes rather than JavaScript
- **SSE (Server-Sent Events)**: A server push technology enabling servers to send real-time updates to clients over HTTP
- **Hypermedia**: HTML content that includes both data and controls (links, forms) for interacting with the application
- **Frontend System**: The client-side web application including HTML, CSS, and JavaScript
- **Backend System**: The Common Lisp server application that handles requests and generates responses
- **Feature Handler**: A server-side component that processes requests for a specific feature (e.g., monster-diagnostic, story-generator)
- **HTML Fragment**: A partial HTML document that can be inserted into the DOM
- **Streaming Response**: A response that sends data incrementally over time rather than all at once
- **HTMX SSE Extension**: An official HTMX extension that provides Server-Sent Events support through HTML attributes

## Requirements

### Requirement 1

**User Story:** As a developer, I want the server to generate HTML responses instead of JSON, so that the frontend follows HTMX's hypermedia-driven architecture

#### Acceptance Criteria

1. WHEN a user submits a form for any feature, THE Backend System SHALL return HTML fragments instead of JSON responses
2. WHEN a feature requires dynamic content, THE Backend System SHALL generate complete HTML markup including all necessary structure and styling classes
3. WHEN rendering feature-specific UI, THE Backend System SHALL include all localized text content in the HTML response
4. WHERE a feature supports multiple languages, THE Backend System SHALL generate HTML with appropriate language-specific content based on request parameters
5. WHEN a non-streaming request completes, THE Backend System SHALL return a complete HTML fragment ready for DOM insertion

### Requirement 2

**User Story:** As a developer, I want to use HTMX's official SSE extension for streaming responses, so that streaming functionality is handled declaratively through HTML attributes

#### Acceptance Criteria

1. WHEN implementing streaming features, THE Frontend System SHALL use the HTMX SSE extension with sse-connect and sse-swap attributes
2. WHEN a streaming connection is established, THE Backend System SHALL send HTML fragments as SSE events with appropriate event names
3. WHEN streaming content updates, THE HTMX SSE Extension SHALL automatically swap the HTML fragments into the designated target elements
4. WHEN a streaming response completes, THE Backend System SHALL send an SSE event indicating completion
5. IF a streaming error occurs, THEN THE Backend System SHALL send an HTML fragment containing error information via SSE

### Requirement 3

**User Story:** As a developer, I want to eliminate client-side rendering logic, so that the codebase is simpler and follows the principle of Locality of Behaviour

#### Acceptance Criteria

1. THE Frontend System SHALL remove all JavaScript functions that generate HTML strings or DOM elements
2. THE Frontend System SHALL remove the feature-content-templates.js file containing client-side HTML templates
3. THE Frontend System SHALL remove the feature-ui-handlers.js file containing client-side rendering logic
4. THE Frontend System SHALL remove the streaming-manager.js custom EventSource implementation
5. THE Frontend System SHALL reduce JavaScript files to only essential functionality not provided by HTMX

### Requirement 4

**User Story:** As a developer, I want feature switching to be handled through HTMX requests, so that the server controls what content is displayed

#### Acceptance Criteria

1. WHEN a user clicks a feature tab, THE Frontend System SHALL trigger an HTMX request to load the feature content
2. WHEN the server receives a feature request, THE Backend System SHALL return a complete HTML fragment for that feature including forms and UI elements
3. WHEN switching features, THE HTMX Library SHALL replace the content area with the server-provided HTML
4. THE Backend System SHALL include all necessary HTMX attributes in the returned HTML for subsequent interactions
5. WHEN a feature is loaded, THE Backend System SHALL include appropriate hx-ext and SSE attributes for streaming support if applicable

### Requirement 5

**User Story:** As a developer, I want conversation history to be managed server-side, so that state management is centralized and consistent

#### Acceptance Criteria

1. WHEN a user sends a chat message, THE Backend System SHALL store the conversation history in server-side session storage
2. WHEN rendering chat UI, THE Backend System SHALL include the complete conversation history in the HTML response
3. WHEN a streaming chat response is received, THE Backend System SHALL append the new message to the server-side history
4. THE Backend System SHALL maintain separate conversation histories for each feature that requires it
5. WHEN a user switches features, THE Backend System SHALL preserve the conversation history for later retrieval

### Requirement 6

**User Story:** As a developer, I want language switching to trigger server-side re-rendering, so that all content is properly localized without client-side manipulation

#### Acceptance Criteria

1. WHEN a user toggles the language selector, THE Frontend System SHALL send an HTMX request with the selected language parameter
2. WHEN the server receives a language change request, THE Backend System SHALL update the session language preference
3. WHEN rendering any content, THE Backend System SHALL use the session language preference to generate localized HTML
4. WHEN language changes, THE Backend System SHALL return updated HTML for all visible content areas
5. THE Backend System SHALL include language-specific text in all HTML responses without requiring client-side translation

### Requirement 7

**User Story:** As a developer, I want error handling to be managed through HTMX response targets, so that errors are displayed consistently without custom JavaScript

#### Acceptance Criteria

1. WHEN a request fails, THE Backend System SHALL return an appropriate HTTP status code with an HTML error fragment
2. WHEN using the HTMX response-targets extension, THE Frontend System SHALL direct error responses to designated error display elements
3. WHEN a streaming error occurs, THE Backend System SHALL send an SSE event containing an HTML error message
4. THE Backend System SHALL generate user-friendly error messages in the appropriate language
5. WHEN an error is displayed, THE Backend System SHALL include retry or fallback options in the HTML response

### Requirement 8

**User Story:** As a developer, I want the initial page load to include only the shell HTML, so that all feature content is loaded dynamically through HTMX

#### Acceptance Criteria

1. THE Frontend System SHALL serve an index.html containing only the page structure, header, footer, and feature tabs
2. WHEN the page loads, THE Frontend System SHALL trigger an HTMX request to load the default feature content
3. THE Backend System SHALL provide endpoints for each feature that return complete HTML fragments
4. THE Frontend System SHALL include HTMX attributes on tabs to load feature content on click
5. WHEN a feature is loaded, THE Backend System SHALL return HTML that includes all necessary forms, inputs, and interaction elements

### Requirement 9

**User Story:** As a developer, I want streaming indicators to be managed through CSS and HTMX classes, so that loading states are handled declaratively

#### Acceptance Criteria

1. WHEN an HTMX request is in progress, THE HTMX Library SHALL automatically add the htmx-request class to the requesting element
2. THE Frontend System SHALL use CSS to show loading indicators based on the htmx-request class
3. WHEN a streaming connection is active, THE Backend System SHALL include HTML elements with appropriate classes for streaming state indication
4. THE Frontend System SHALL use CSS to style streaming indicators without JavaScript manipulation
5. WHEN a request completes, THE HTMX Library SHALL automatically remove the htmx-request class

### Requirement 10

**User Story:** As a developer, I want to maintain the existing feature functionality, so that users experience no regression in capabilities

#### Acceptance Criteria

1. THE Refactored System SHALL support all five existing features with identical functionality
2. THE Refactored System SHALL support both streaming and non-streaming modes for all applicable features
3. THE Refactored System SHALL maintain conversation history for character-chat and trivia-bot features
4. THE Refactored System SHALL support language switching between Japanese and English
5. THE Refactored System SHALL maintain mobile responsiveness and accessibility
