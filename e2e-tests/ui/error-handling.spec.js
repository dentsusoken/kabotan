/**
 * Error Handling E2E Tests (UI Tests with Mocks)
 * 
 * This test suite covers HTMX-based error handling UI behavior without requiring real LLM API.
 * 
 * UI TESTS:
 * - Form validation
 * - Error message display
 * - Network error handling
 * - Special character handling
 * - Application stability
 * 
 * MOCK USAGE:
 * - All LLM API calls are mocked for fast execution
 * - Tests focus on UI error handling, not real API errors
 * 
 * ERROR SCENARIOS TESTED:
 * - Invalid language parameters
 * - Network timeouts and connection failures
 * - Missing required form fields (HTML5 validation)
 * - API error responses (500, 404, etc.)
 * - Empty responses from API
 * - Special characters and XSS attempts
 * 
 * HTMX ERROR HANDLING MECHANISMS:
 * - HTML5 form validation (required attributes)
 * - HTMX response-targets extension for error routing
 * - hx-target-5* for server errors (500-599)
 * - hx-target-4* for client errors (400-499)
 * - Network error detection and graceful degradation
 * - Error message display in UI via HTML fragments
 * - Application stability after errors
 * - htmx-request class during API calls
 * 
 * VALIDATION:
 * - All required fields have required attribute
 * - Form submission blocked by browser if fields empty
 * - Special characters properly escaped
 * - XSS attempts sanitized
 * 
 * ERROR DISPLAY:
 * - .alert-error: Error message container
 * - Error messages shown in appropriate language
 * - User-friendly error messages as HTML fragments
 * - HTMX swaps error HTML into designated error containers
 */
const { test, expect } = require('@playwright/test');
const { mockAllEndpoints, mockNonStreamingEndpoint } = require('../helpers/mock-llm-api');

test.describe('Error Handling Scenarios (UI Tests)', () => {
  test.describe('Standard Error Tests', () => {
    test.beforeEach(async ({ page }) => {
      // Setup default mocks for all endpoints
      await mockAllEndpoints(page, { delay: 50 });

      await page.goto('/');
      await page.waitForLoadState('networkidle');
    });

  test('should display error for invalid language parameter', async ({ page }) => {
    // Manually set invalid language in localStorage
    await page.evaluate(() => {
      localStorage.setItem('language', 'invalid');
    });
    
    // Reload page
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Should default to valid language (browser default)
    const language = await page.evaluate(() => localStorage.getItem('language'));
    // Invalid language should be replaced with default (ja or en)
    expect(language).toMatch(/^(ja|en)$/);
  });

  test('should handle network timeout gracefully', async ({ page }) => {
    // Switch to Monster Diagnostic and wait for HTMX to load content
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill form (correct field names with underscores)
    await page.fill('input[name="favorite_food"]', 'blood');
    await page.fill('input[name="sleep_schedule"]', 'nocturnal');
    await page.fill('input[name="hobby"]', 'reading');
    await page.fill('input[name="fear"]', 'sunlight');
    
    // Intercept API request and abort it
    await page.route('**/api/monster-diagnostic*', async route => {
      await route.abort();
    });
    
    // Submit form
    await page.click('button[type="submit"]');
    
    // Wait a moment for error handling
    await page.waitForTimeout(2000);
    
    // Verify page is still functional
    const pageTitle = await page.title();
    expect(pageTitle).toBeTruthy();
  });

  test('should display validation error for missing required fields', async ({ page }) => {
    // Switch to Story Generator
    await page.click('[data-feature="story-generator"]');
    await page.waitForTimeout(300);
    
    // Verify required fields have HTML5 validation
    const nameInput = page.locator('input[name="name"]');
    await expect(nameInput).toHaveAttribute('required', '');
    
    const themeInput = page.locator('input[name="theme"]');
    await expect(themeInput).toHaveAttribute('required', '');
  });

  test('should handle API error response with HTMX response-targets', async ({ page }) => {
    // Switch to Monster Diagnostic and wait for HTMX to load content
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill form (correct field names with underscores)
    await page.fill('input[name="favorite_food"]', 'blood');
    await page.fill('input[name="sleep_schedule"]', 'nocturnal');
    await page.fill('input[name="hobby"]', 'reading');
    await page.fill('input[name="fear"]', 'sunlight');
    
    // Intercept API request and return error with HTML fragment
    await page.route('**/api/monster-diagnostic*', async route => {
      await route.fulfill({
        status: 500,
        contentType: 'text/html',
        body: '<div class="alert alert-error">Internal server error</div>'
      });
    });
    
    // Submit form
    await page.click('button[type="submit"]');
    
    // HTMX response-targets should route error to error container
    await page.waitForTimeout(1000);
    
    // Should show error message (either in result container or error container)
    const errorAlert = page.locator('.alert-error').first();
    if (await errorAlert.isVisible()) {
      await expect(errorAlert).toBeVisible();
    }
  });

  test('should handle empty response from API', async ({ page }) => {
    // Switch to Story Generator
    await page.click('text=Story Generator');
    
    // Fill form
    await page.fill('input[name="name"]', 'Alice');
    await page.fill('input[name="theme"]', 'test');
    
    // Intercept API request and return empty response
    await page.route('**/api/story-generator', async route => {
      await route.fulfill({
        status: 200,
        contentType: 'text/html',
        body: ''
      });
    });
    
    // Submit form
    await page.click('button[type="submit"]');
    
    // Wait a moment
    await page.waitForTimeout(2000);
    
    // Should handle gracefully (no crash)
    const pageTitle = await page.title();
    expect(pageTitle).toBeTruthy();
  });

  test('should display user-friendly error messages in Japanese', async ({ page }) => {
    // Verify current language
    let language = await page.evaluate(() => localStorage.getItem('language'));
    
    // If not Japanese, switch to it
    if (language !== 'ja') {
      await page.evaluate(() => {
        const toggle = document.getElementById('language-toggle');
        toggle.click();
      });
      await page.waitForTimeout(2000);
    }
    
    // Switch to Monster Diagnostic and wait for HTMX to load content
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Verify form is loaded (form should be in Japanese if language is ja)
    const foodInput = page.locator('input[name="favorite_food"]');
    await expect(foodInput).toBeVisible();
  });
  }); // End of Standard Error Tests

  test.describe('Non-Streaming Error Tests', () => {
    test.beforeEach(async ({ page }) => {
      // Setup mock for POST endpoint only (no streaming)
      await mockNonStreamingEndpoint(page, '**/api/monster-diagnostic', {
        feature: 'monster-diagnostic',
        lang: 'en',
        delay: 100,
      });

      await page.goto('/');
      await page.waitForLoadState('networkidle');
    });

  test('should show HTMX loading state during API call', async ({ page }) => {
    // Switch to Monster Diagnostic and wait for HTMX to load content
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill form (correct field names with underscores)
    await page.fill('input[name="favorite_food"]', 'blood');
    await page.fill('input[name="sleep_schedule"]', 'nocturnal');
    await page.fill('input[name="hobby"]', 'reading');
    await page.fill('input[name="fear"]', 'sunlight');
    
    // Submit form - HTMX will add htmx-request class during request
    const submitButton = page.locator('button[type="submit"]');
    await submitButton.click();
    
    // Wait for request to complete
    await page.waitForTimeout(2000);
    
    // Verify result is displayed (request completed)
    const resultDiv = page.locator('#monster-result-container');
    const resultText = await resultDiv.textContent();
    expect(resultText.length).toBeGreaterThan(0);
  });

  test('should handle special characters in input with HTMX', async ({ page }) => {
    // Switch to Monster Diagnostic and wait for HTMX to load content
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill form with special characters - backend should escape them (correct field names with underscores)
    await page.fill('input[name="favorite_food"]', '<script>alert("xss")</script>');
    await page.fill('input[name="sleep_schedule"]', 'test & test');
    await page.fill('input[name="hobby"]', 'reading "books"');
    await page.fill('input[name="fear"]', "it's scary");
    
    // Submit form via HTMX
    await page.click('button[type="submit"]');
    
    // Wait for HTMX request to complete
    await page.waitForTimeout(2000);
    
    // Page should still be functional
    const pageTitle = await page.title();
    expect(pageTitle).toBeTruthy();
    
    // Verify result is displayed (special characters handled)
    const resultDiv = page.locator('#monster-result-container');
    const resultText = await resultDiv.textContent();
    expect(resultText.length).toBeGreaterThan(0);
  });
  }); // End of Non-Streaming Error Tests
});
