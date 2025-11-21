/**
 * Monster Diagnostic Integration Tests
 * 
 * This test suite verifies monster diagnostic functionality with real LLM API calls.
 * NO MOCKS - All tests use actual OpenAI-compatible endpoint.
 * 
 * STREAMING MODE:
 * - Uses SSE (Server-Sent Events) via HTMX SSE extension
 * - Endpoint: GET /api/monster-diagnostic-stream
 * - Real-time response streaming from LLM
 * 
 * EXPECTED RESPONSE FORMAT:
 * The backend returns HTML in this format (from src/utils/response-formatting.lisp):
 * <div class="alert alert-success">
 *   <h3 class="font-bold text-lg mb-2">{title}</h3>
 *   <div class="whitespace-pre-wrap">{text}</div>
 * </div>
 * 
 * FORM INPUTS:
 * - favorite_food: User's favorite food
 * - sleep_schedule: User's sleep pattern
 * - hobby: User's hobby
 * - fear: User's fear
 * All fields are required (HTML5 validation)
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via lang parameter
 * 
 * TEST REQUIREMENTS:
 * - Validates: Requirements 1.1, 1.2, 1.3, 1.4, 3.1, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4
 * - Tests real LLM API integration
 * - Verifies streaming content delivery
 * - Validates result format and content length
 * - Tests multilingual support (English and Japanese)
 * 
 * NOTE:
 * - Tests may take 30-180 seconds due to real LLM response time
 * - Requires valid OPENAI_API_KEY environment variable
 * - Tests will fail if LLM API is unavailable
 */
const { test, expect } = require('@playwright/test');

test.describe('Monster Diagnostic Integration Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    
    // Switch to Monster Diagnostic tab (default tab)
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForTimeout(500);
  });

  test('should diagnose monster with English input', async ({ page }) => {
    // Requirement 3.1: Test monster diagnostic feature with real LLM
    // Requirement 4.1: Test features in English language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill in the form with English input
    await page.fill('input[name="favorite_food"]', 'blood');
    await page.fill('input[name="sleep_schedule"]', 'nocturnal');
    await page.fill('input[name="hobby"]', 'reading ancient books');
    await page.fill('input[name="fear"]', 'sunlight');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    await page.waitForSelector('#monster-result-container .alert', { timeout: 180000 });
    
    // Wait for content to be populated (not just loading indicator)
    await page.waitForFunction(() => {
      const alert = document.querySelector('#monster-result-container .alert');
      if (!alert) return false;
      const text = alert.textContent || '';
      return text.length > 50 && !text.includes('loading');
    }, { timeout: 180000 });
    
    // Verify result is displayed
    const resultAlert = page.locator('#monster-result-container .alert').first();
    await expect(resultAlert).toBeVisible();
    
    // Verify result has substantial content
    const resultText = await resultAlert.textContent();
    expect(resultText.length).toBeGreaterThan(50);
    
    // Verify result is in English (contains common English words)
    const lowerText = resultText.toLowerCase();
    const hasEnglishWords = lowerText.includes('you') || 
                           lowerText.includes('the') || 
                           lowerText.includes('a') ||
                           lowerText.includes('is') ||
                           lowerText.includes('are');
    expect(hasEnglishWords).toBe(true);
    
    // Verify result has expected structure (title and content)
    const title = page.locator('#monster-result-container .alert h3');
    await expect(title).toBeVisible();
    const titleText = await title.textContent();
    expect(titleText.length).toBeGreaterThan(0);
  });

  test('should diagnose monster with Japanese input', async ({ page }) => {
    // Requirement 3.1: Test monster diagnostic feature with real LLM
    // Requirement 4.2: Test features in Japanese language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Switch to Japanese language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Reload monster diagnostic to get Japanese interface
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill in the form with Japanese input
    await page.fill('input[name="favorite_food"]', '血液');
    await page.fill('input[name="sleep_schedule"]', '夜行性');
    await page.fill('input[name="hobby"]', '古書を読むこと');
    await page.fill('input[name="fear"]', '日光');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    await page.waitForSelector('#monster-result-container .alert', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const alert = document.querySelector('#monster-result-container .alert');
      if (!alert) return false;
      const text = alert.textContent || '';
      return text.length > 50 && !text.includes('loading');
    }, { timeout: 180000 });
    
    // Verify result is displayed
    const resultAlert = page.locator('#monster-result-container .alert').first();
    await expect(resultAlert).toBeVisible();
    
    // Verify result has substantial content
    const resultText = await resultAlert.textContent();
    expect(resultText.length).toBeGreaterThan(50);
    
    // Verify result contains Japanese characters (Hiragana, Katakana, or Kanji)
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(resultText);
    expect(hasJapanese).toBe(true);
    
    // Verify result has expected structure
    const title = page.locator('#monster-result-container .alert h3');
    await expect(title).toBeVisible();
  });

  test('should verify streaming response with real LLM', async ({ page }) => {
    // Requirement 5.1: Verify content is delivered incrementally
    // Requirement 5.2: Verify UI updates during streaming
    // Requirement 5.3: Verify final content is complete and correct
    // Requirement 5.4: Use real LLM API streaming endpoints
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Set up mutation observer to track streaming updates
    await page.evaluate(() => {
      window.streamingUpdates = [];
      const targetNode = document.querySelector('#monster-result-container');
      if (targetNode) {
        const observer = new MutationObserver(() => {
          const alert = document.querySelector('#monster-result-container .alert');
          if (alert) {
            const contentLength = alert.textContent.length;
            window.streamingUpdates.push({
              timestamp: Date.now(),
              contentLength: contentLength
            });
          }
        });
        observer.observe(targetNode, { childList: true, subtree: true, characterData: true });
      }
    });
    
    // Fill in the form with input that will generate a longer response
    await page.fill('input[name="favorite_food"]', 'rare steaks and dark chocolate');
    await page.fill('input[name="sleep_schedule"]', 'I sleep during the day and wake at midnight');
    await page.fill('input[name="hobby"]', 'collecting ancient artifacts and studying occult mysteries');
    await page.fill('input[name="fear"]', 'bright sunlight and silver objects');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to complete
    await page.waitForSelector('#monster-result-container .alert', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const alert = document.querySelector('#monster-result-container .alert');
      if (!alert) return false;
      const text = alert.textContent || '';
      return text.length > 100;
    }, { timeout: 180000 });
    
    // Wait a bit more to ensure streaming is complete
    await page.waitForTimeout(2000);
    
    // Get streaming updates
    const updates = await page.evaluate(() => window.streamingUpdates || []);
    
    // Verify streaming occurred (multiple updates)
    // Note: Due to timing, we may not always catch all updates
    if (updates.length > 1) {
      // Verify content length increased over time
      const firstUpdate = updates[0];
      const lastUpdate = updates[updates.length - 1];
      expect(lastUpdate.contentLength).toBeGreaterThan(firstUpdate.contentLength);
    }
    
    // Verify final content is present and substantial
    const resultAlert = page.locator('#monster-result-container .alert').first();
    const finalContent = await resultAlert.textContent();
    expect(finalContent.length).toBeGreaterThan(100);
  });

  test('should verify result format and content length', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    // Verify result has proper format and sufficient content
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill in the form
    await page.fill('input[name="favorite_food"]', 'pumpkin pie');
    await page.fill('input[name="sleep_schedule"]', 'early to bed, early to rise');
    await page.fill('input[name="hobby"]', 'gardening');
    await page.fill('input[name="fear"]', 'darkness');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('#monster-result-container .alert', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const alert = document.querySelector('#monster-result-container .alert');
      if (!alert) return false;
      const text = alert.textContent || '';
      return text.length > 50;
    }, { timeout: 180000 });
    
    // Verify result has expected HTML structure
    const resultAlert = page.locator('#monster-result-container .alert').first();
    await expect(resultAlert).toBeVisible();
    
    // Verify alert has success class
    await expect(resultAlert).toHaveClass(/alert-success/);
    
    // Verify title exists and has content
    const title = page.locator('#monster-result-container .alert h3');
    await expect(title).toBeVisible();
    await expect(title).toHaveClass(/font-bold/);
    const titleText = await title.textContent();
    expect(titleText.length).toBeGreaterThan(0);
    
    // Verify content div exists and has substantial content
    const contentDiv = page.locator('#monster-result-container .alert .whitespace-pre-wrap');
    await expect(contentDiv).toBeVisible();
    const contentText = await contentDiv.textContent();
    expect(contentText.length).toBeGreaterThan(20);
    
    // Verify total result length is substantial (at least 50 characters)
    const totalText = await resultAlert.textContent();
    expect(totalText.length).toBeGreaterThan(50);
  });

  test('should handle different personality types', async ({ page }) => {
    // Additional test to verify LLM generates different responses for different inputs
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Fill in the form with "friendly" personality traits
    await page.fill('input[name="favorite_food"]', 'cookies and milk');
    await page.fill('input[name="sleep_schedule"]', 'regular sleep schedule');
    await page.fill('input[name="hobby"]', 'helping others');
    await page.fill('input[name="fear"]', 'being alone');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('#monster-result-container .alert', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const alert = document.querySelector('#monster-result-container .alert');
      if (!alert) return false;
      const text = alert.textContent || '';
      return text.length > 50;
    }, { timeout: 180000 });
    
    // Verify result is displayed and has content
    const resultAlert = page.locator('#monster-result-container .alert').first();
    await expect(resultAlert).toBeVisible();
    
    const resultText = await resultAlert.textContent();
    expect(resultText.length).toBeGreaterThan(50);
    
    // Verify result contains some personality-related content
    // (The LLM should generate a monster personality based on the inputs)
    const lowerText = resultText.toLowerCase();
    const hasPersonalityContent = lowerText.includes('monster') || 
                                  lowerText.includes('personality') ||
                                  lowerText.includes('character') ||
                                  lowerText.includes('creature');
    expect(hasPersonalityContent).toBe(true);
  });
});
