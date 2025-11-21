/**
 * Story Generator E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers the story generator UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Form element visibility and validation
 * - Style selection interface
 * - Input field validation
 * - HTML structure verification
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI interactions, not LLM responses
 * 
 * EXPECTED RESPONSE FORMAT:
 * The backend returns HTML in this format (from src/utils/response-formatting.lisp):
 * <div class="bg-base-300 rounded-lg p-4">
 *   <h3 class="font-bold text-lg mb-2">{title}</h3>
 *   <div class="whitespace-pre-wrap">{text}</div>
 * </div>
 * 
 * FORM INPUTS:
 * - name: Character name for the story (required)
 * - theme: Story theme (required)
 * - style: Story style (radio buttons: gothic, parody, classic)
 * 
 * STYLE OPTIONS:
 * - Gothic: Dark, atmospheric horror stories
 * - Parody: Humorous, lighthearted Halloween stories
 * - Classic: Traditional Halloween tales
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 */
const { test, expect } = require('@playwright/test');

test.describe('Story Generator Feature (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display story generator form', async ({ page }) => {
    // Switch to Story Generator tab
    await page.click('[data-feature="story-generator"]');
    await page.waitForTimeout(500);
    
    // Verify form elements are visible
    await expect(page.locator('input[name="name"]')).toBeVisible();
    await expect(page.locator('input[name="theme"]')).toBeVisible();
    await expect(page.locator('input[name="style"][value="gothic"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test('should show validation error for empty name', async ({ page }) => {
    // Switch to Story Generator tab
    await page.click('[data-feature="story-generator"]');
    
    // Wait for HTMX to load the content
    await page.waitForSelector('input[name="name"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Verify name field is required (HTML5 validation)
    const nameInput = page.locator('input[name="name"]');
    await expect(nameInput).toHaveAttribute('required', '');
    
    const isRequired = await nameInput.evaluate(el => el.hasAttribute('required'));
    expect(isRequired).toBe(true);
  });
});
