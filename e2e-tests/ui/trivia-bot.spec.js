/**
 * Trivia Bot E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers the trivia bot UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Form element visibility and validation
 * - Input field validation
 * - HTML structure verification
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI interactions, not LLM responses
 * 
 * EXPECTED RESPONSE FORMAT:
 * The backend returns HTML in this format (from src/utils/response-formatting.lisp):
 * <div class="chat chat-start">
 *   <div class="chat-header">Trivia Bot</div>
 *   <div class="chat-bubble chat-bubble-secondary">{text}</div>
 * </div>
 * 
 * FORM INPUTS:
 * - question: User's trivia question
 * Field is required (HTML5 validation)
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 */
const { test, expect } = require('@playwright/test');

test.describe('Trivia Bot Feature (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display trivia bot interface', async ({ page }) => {
    // Switch to Trivia Bot tab
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(300);
    
    // Verify form elements are visible
    await expect(page.locator('input[name="question"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test('should show validation error for empty question', async ({ page }) => {
    // Switch to Trivia Bot tab
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(300);
    
    // Try to submit without question (HTML5 validation will prevent this)
    const questionInput = page.locator('input[name="question"]');
    await expect(questionInput).toHaveAttribute('required', '');
    
    // Verify the input is required
    const isRequired = await questionInput.evaluate(el => el.hasAttribute('required'));
    expect(isRequired).toBe(true);
  });
});
