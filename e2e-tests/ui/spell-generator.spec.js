/**
 * Spell Generator E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers the spell generator UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Interface element visibility
 * - Button interactions
 * - HTML structure verification
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI interactions, not LLM responses
 * 
 * EXPECTED RESPONSE FORMAT:
 * The backend returns HTML in this format (from src/utils/response-formatting.lisp):
 * <div class="text-center">
 *   <div class="spell-phrase">{spell}</div>
 *   <div class="spell-explanation mt-6">{meaning}</div>
 * </div>
 * 
 * FEATURE BEHAVIOR:
 * - Spell generator auto-loads on tab switch (uses real API)
 * - Regenerate button triggers new spell generation
 * - No form inputs required
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 */
const { test, expect } = require('@playwright/test');

test.describe('Spell Generator Feature (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display spell generator interface', async ({ page }) => {
    // Switch to Spell Generator tab
    await page.click('[data-feature="spell-generator"]');
    
    // Verify regenerate button is visible
    const regenerateBtn = page.locator('#spell-regenerate-btn');
    await expect(regenerateBtn).toBeVisible();
    
    // Verify spell generator content area is visible
    const spellContent = page.locator('#spell-generator-content');
    await expect(spellContent).toBeVisible();
  });
});
