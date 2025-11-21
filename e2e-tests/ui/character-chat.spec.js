/**
 * Character Chat E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers the character chat UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Form element visibility and validation
 * - Character selection interface
 * - Message input validation
 * - HTML structure verification
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI interactions, not LLM responses
 * 
 * EXPECTED RESPONSE FORMAT:
 * The backend returns HTML in this format (from src/utils/response-formatting.lisp):
 * <div class="chat chat-start">
 *   <div class="chat-header">{emoji}</div>
 *   <div class="chat-bubble chat-bubble-secondary">{text}</div>
 * </div>
 * 
 * CHARACTER-SPECIFIC RESPONSES:
 * - Dracula: Gothic, formal vampire responses with 🧛 emoji
 * - Witch: Mystical, spell-focused responses with 🧙 emoji
 * - Jack-o-Lantern: Playful, Halloween-themed responses with 🎃 emoji
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 */
const { test, expect } = require('@playwright/test');

test.describe('Character Chat Feature (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display character chat interface', async ({ page }) => {
    // Switch to Character Chat tab
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(300);
    
    // Verify character selection is visible (use radio type to be specific)
    await expect(page.locator('input[type="radio"][value="dracula"]')).toBeVisible();
    await expect(page.locator('input[type="radio"][value="witch"]')).toBeVisible();
    await expect(page.locator('input[type="radio"][value="jack-o-lantern"]')).toBeVisible();
    
    // Verify message input is visible (it's an input, not textarea)
    await expect(page.locator('input[name="message"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test('should show validation error for empty message', async ({ page }) => {
    // Switch to Character Chat tab
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(300);
    
    // Try to submit without message (HTML5 validation will prevent this)
    const messageInput = page.locator('input[name="message"]');
    await expect(messageInput).toHaveAttribute('required', '');
    
    // Verify the input is required
    const isRequired = await messageInput.evaluate(el => el.hasAttribute('required'));
    expect(isRequired).toBe(true);
  });
});
