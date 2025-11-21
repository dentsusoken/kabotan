/**
 * Language Switching E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers language switching UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Language selector visibility
 * - Language switching functionality
 * - UI element updates
 * - localStorage persistence
 * - Browser language detection
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI language switching, not API responses
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default for English-speaking browsers
 * - Japanese (ja): Default for Japanese-speaking browsers
 * - Language stored in localStorage with key 'language'
 * - Language parameter sent with all API requests
 * 
 * LANGUAGE SWITCHING:
 * - Toggle switch in header (#language-toggle)
 * - Switches between 'en' and 'ja'
 * - Updates all UI elements with data-i18n attributes
 * - Persists preference in localStorage
 * - Detected from browser language on first load
 * 
 * I18N ELEMENTS:
 * - data-i18n attributes: Mark translatable elements
 * - Tab labels: tab-monster, tab-story, tab-chat, tab-trivia, tab-spell
 * - Form labels: monster-food-label, etc.
 * - Button text: Clear Chat, Submit, etc.
 * 
 * BROWSER LANGUAGE DETECTION:
 * - Accept-Language header used on first load
 * - Falls back to 'en' if language not supported
 * - User preference overrides browser language
 */
const { test, expect } = require('@playwright/test');
const { mockAllEndpoints } = require('../helpers/mock-llm-api');

test.describe('Language Switching Functionality (UI Tests)', () => {
  test.beforeEach(async ({ page }) => {
    // Setup mocks for all endpoints to speed up tests
    await mockAllEndpoints(page, { delay: 50 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should display language selector', async ({ page }) => {
    // Verify language selector is visible
    await expect(page.locator('text=English')).toBeVisible();
    await expect(page.locator('text=日本語')).toBeVisible();
  });

  test('should switch from English to Japanese', async ({ page }) => {
    // Get initial language
    const initialLang = await page.evaluate(() => localStorage.getItem('language'));
    
    // Verify tab is visible
    const monsterTab = page.locator('[data-i18n="tab-monster"]');
    await expect(monsterTab).toBeVisible();
    
    // Toggle language by clicking the checkbox directly with JavaScript
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify language changed
    const newLang = await page.evaluate(() => localStorage.getItem('language'));
    expect(newLang).not.toBe(initialLang);
    expect(['ja', 'en']).toContain(newLang);
  });

  test('should switch from Japanese to English', async ({ page }) => {
    // Get initial language
    let currentLang = await page.evaluate(() => localStorage.getItem('language'));
    const initialLang = currentLang;
    
    // Switch language by toggling
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify language changed
    currentLang = await page.evaluate(() => localStorage.getItem('language'));
    expect(currentLang).not.toBe(initialLang);
    
    // Switch back
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify language changed back
    currentLang = await page.evaluate(() => localStorage.getItem('language'));
    expect(currentLang).toBe(initialLang);
  });

  test('should update all feature tabs when switching language', async ({ page }) => {
    // Get initial tab text
    const monsterTab = page.locator('[data-i18n="tab-monster"]');
    const initialText = await monsterTab.textContent();
    
    // Switch language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify tab text changed
    const newText = await monsterTab.textContent();
    expect(newText).not.toBe(initialText);
    
    // Verify all tabs are visible and have text
    await expect(page.locator('[data-i18n="tab-monster"]')).toBeVisible();
    await expect(page.locator('[data-i18n="tab-story"]')).toBeVisible();
    await expect(page.locator('[data-i18n="tab-chat"]')).toBeVisible();
    await expect(page.locator('[data-i18n="tab-trivia"]')).toBeVisible();
    await expect(page.locator('[data-i18n="tab-spell"]')).toBeVisible();
  });

  test('should persist language preference in localStorage', async ({ page }) => {
    // Get initial language
    const initialLang = await page.evaluate(() => localStorage.getItem('language'));
    
    // Switch language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Check localStorage changed
    let language = await page.evaluate(() => localStorage.getItem('language'));
    expect(language).not.toBe(initialLang);
    const changedLang = language;
    
    // Reload page
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Verify language persisted
    language = await page.evaluate(() => localStorage.getItem('language'));
    expect(language).toBe(changedLang);
  });

  test('should detect browser language on first load', async ({ page, context }) => {
    // Create new context with English locale
    const enPage = await context.newPage();
    
    await enPage.setExtraHTTPHeaders({
      'Accept-Language': 'en-US,en;q=0.9'
    });
    
    // Setup mocks for the new page (after setting headers)
    await mockAllEndpoints(enPage, { delay: 50 });
    
    // Navigate to page (language detection happens on first load)
    await enPage.goto('/');
    await enPage.waitForLoadState('networkidle');
    
    // Should default to English based on browser language
    const language = await enPage.evaluate(() => localStorage.getItem('language'));
    expect(language).toBe('en');
    
    await enPage.close();
    
    // Re-setup mocks for the original page after closing the new page
    // This ensures that mocks are still active for subsequent tests
    await mockAllEndpoints(page, { delay: 50 });
  });

  test('should reload feature content when switching language', async ({ page }) => {
    // Check current language
    let currentLang = await page.evaluate(() => localStorage.getItem('language'));
    
    // Go to Monster Diagnostic and wait for HTMX to load the content
    await page.click('[data-feature="monster-diagnostic"]');
    
    // Wait for form to be loaded by HTMX (correct field name with underscore)
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Verify form is loaded
    const foodInput = page.locator('input[name="favorite_food"]');
    await expect(foodInput).toBeVisible();
    
    // Toggle language - this should trigger HTMX to reload the feature content
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    
    // Wait for HTMX to reload the feature content
    await page.waitForTimeout(2000);
    
    // Verify form is still visible (reloaded with new language)
    await expect(foodInput).toBeVisible();
    
    // Verify language changed in localStorage
    const newLang = await page.evaluate(() => localStorage.getItem('language'));
    expect(newLang).not.toBe(currentLang);
  });
});
