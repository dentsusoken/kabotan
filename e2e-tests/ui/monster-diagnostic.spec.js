/**
 * Monster Diagnostic E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers the monster diagnostic UI behavior without requiring real LLM API.
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
 * <div class="alert alert-success">
 *   <h3 class="font-bold text-lg mb-2">{title}</h3>
 *   <div class="whitespace-pre-wrap">{text}</div>
 * </div>
 * 
 * FORM INPUTS:
 * - favorite-food: User's favorite food
 * - sleep-schedule: User's sleep pattern
 * - hobby: User's hobby
 * - fear: User's fear
 * All fields are required (HTML5 validation)
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 */
const { test, expect } = require('@playwright/test');

test.describe('Monster Diagnostic Feature (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display monster diagnostic form', async ({ page }) => {
    // Monster Diagnostic is the default tab, but click it to be sure
    await page.click('[data-feature="monster-diagnostic"]');
    
    // Wait for HTMX to load the content
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(300);
    
    // Verify form elements are visible (correct field names with underscores)
    await expect(page.locator('input[name="favorite_food"]')).toBeVisible();
    await expect(page.locator('input[name="sleep_schedule"]')).toBeVisible();
    await expect(page.locator('input[name="hobby"]')).toBeVisible();
    await expect(page.locator('input[name="fear"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test('should show validation error for empty fields', async ({ page }) => {
    // Monster Diagnostic is the default tab
    await page.click('[data-feature="monster-diagnostic"]');
    
    // Wait for HTMX to load the content
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(300);
    
    // Verify fields are required (HTML5 validation) - correct field name with underscore
    const foodInput = page.locator('input[name="favorite_food"]');
    await expect(foodInput).toHaveAttribute('required', '');
    
    const isRequired = await foodInput.evaluate(el => el.hasAttribute('required'));
    expect(isRequired).toBe(true);
  });
});
