# Requirements Document

## Introduction

This specification addresses two critical issues in the character chat feature:
1. Character selection not being properly transmitted to the backend
2. Poor UI/UX for character selection radio buttons (cramped spacing and misalignment)

## Glossary

- **Character Chat System**: The web application feature that enables users to have conversations with Halloween characters
- **Character Selector**: Radio button group that allows users to choose between Dracula, Witch, and Jack-o'-lantern
- **HTMX Form**: HTML form that uses HTMX attributes for dynamic submission
- **Radio Button Group**: A set of mutually exclusive radio input elements

## Requirements

### Requirement 1

**User Story:** As a user, I want my character selection to be properly transmitted when I submit a chat message, so that I can have conversations with the character I chose.

#### Acceptance Criteria

1. WHEN a user selects a character and submits a message THEN the system SHALL include the selected character value in the form submission
2. WHEN the form is submitted via HTMX streaming endpoint THEN the system SHALL transmit the character parameter in the request
3. WHEN the backend receives the character parameter THEN the system SHALL use that character for generating the response

### Requirement 2

**User Story:** As a user, I want the character selection radio buttons to be visually clear and easy to interact with, so that I can quickly and accurately select my preferred character.

#### Acceptance Criteria

1. WHEN the character selector is displayed THEN the system SHALL provide adequate vertical spacing between radio button options
2. WHEN the character selector is displayed THEN the system SHALL align radio buttons and their labels horizontally
3. WHEN the character selector is displayed THEN the system SHALL ensure sufficient clickable area for each option
4. WHEN the character selector is displayed THEN the system SHALL maintain consistent visual hierarchy with other form elements
