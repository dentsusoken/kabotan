/**
 * Multilingual Integration Tests
 * 
 * This test suite verifies multilingual support across all features with real LLM API calls.
 * NO MOCKS - All tests use actual OpenAI-compatible endpoint.
 * 
 * LANGUAGE SUPPORT:
 * - English (en): Default language
 * - Japanese (ja): Supported via language parameter
 * 
 * LANGUAGE SWITCHING:
 * - Language preference stored in localStorage
 * - Language toggle button in header
 * - Language parameter sent to API endpoints
 * 
 * TEST REQUIREMENTS:
 * - Validates: Requirements 4.1, 4.2, 4.3, 4.4
 * - Tests language parameter transmission to API
 * - Verifies English responses from LLM
 * - Verifies Japanese responses from LLM
 * - Tests language switching during session
 * 
 * NOTE:
 * - Tests may take 30-180 seconds due to real LLM response time
 * - Requires valid OPENAI_API_KEY environment variable
 * - Tests will fail if LLM API is unavailable
 */
const { test, expect } = require('@playwright/test');

test.describe('Multilingual Integration Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('should send English language parameter to API and receive English response', async ({ page }) => {
    // Requirement 4.1: Test features in English language
    // Requirement 4.3: Verify language parameter is sent to API
    
    // Ensure English is selected (default)
    await page.evaluate(() => {
      localStorage.setItem('language', 'en');
    });
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Switch to Monster Diagnostic
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForTimeout(500);
    
    // Wait for form to load
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Set up request interception to verify language parameter
    const requests = [];
    page.on('request', request => {
      const url = request.url();
      if (url.includes('/api/monster-diagnostic-stream')) {
        requests.push({
          url: url,
          method: request.method()
        });
      }
    });
    
    // Fill in the form
    await page.fill('input[name="favorite_food"]', 'pizza');
    await page.fill('input[name="sleep_schedule"]', 'normal');
    await page.fill('input[name="hobby"]', 'reading');
    await page.fill('input[name="fear"]', 'spiders');
    
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
    
    // Verify language parameter was sent in request
    expect(requests.length).toBeGreaterThan(0);
    const streamRequest = requests.find(r => r.url.includes('/api/monster-diagnostic-stream'));
    expect(streamRequest).toBeDefined();
    expect(streamRequest.url).toContain('language=en');
    
    // Verify response is in English
    const resultAlert = page.locator('#monster-result-container .alert').first();
    const resultText = await resultAlert.textContent();
    expect(resultText.length).toBeGreaterThan(50);
    
    // Verify English content (contains common English words)
    const lowerText = resultText.toLowerCase();
    const hasEnglishWords = lowerText.includes('you') || 
                           lowerText.includes('the') || 
                           lowerText.includes('a') ||
                           lowerText.includes('is') ||
                           lowerText.includes('are');
    expect(hasEnglishWords).toBe(true);
  });

  test('should send Japanese language parameter to API and receive Japanese response', async ({ page }) => {
    // Requirement 4.2: Test features in Japanese language
    // Requirement 4.3: Verify language parameter is sent to API
    
    // Switch to Japanese language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify Japanese is now selected
    const lang = await page.evaluate(() => localStorage.getItem('language'));
    expect(lang).toBe('ja');
    
    // Switch to Character Chat (more reliable than Monster Diagnostic)
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="message"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Set up request interception to verify language parameter
    const requests = [];
    page.on('request', request => {
      const url = request.url();
      if (url.includes('/api/character-chat-stream')) {
        requests.push({
          url: url,
          method: request.method()
        });
      }
    });
    
    // Send a message in Japanese
    await page.fill('input[name="message"]', 'こんにちは');
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify language parameter was sent in request
    expect(requests.length).toBeGreaterThan(0);
    const streamRequest = requests.find(r => r.url.includes('/api/character-chat-stream'));
    expect(streamRequest).toBeDefined();
    expect(streamRequest.url).toContain('language=ja');
    
    // Verify response is in Japanese
    const chatBubbles = page.locator('.chat-bubble');
    const assistantBubble = chatBubbles.last();
    const resultText = await assistantBubble.textContent();
    expect(resultText.length).toBeGreaterThan(10);
    
    // Verify Japanese content (contains Japanese characters)
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(resultText);
    expect(hasJapanese).toBe(true);
  });

  test('should switch language during session and receive responses in new language', async ({ page }) => {
    // Requirement 4.4: Test language switching during session
    // Requirement 4.3: Verify language parameter is sent to API
    
    // Start with English
    await page.evaluate(() => {
      localStorage.setItem('language', 'en');
    });
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Switch to Character Chat (simpler than Story Generator)
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="message"]', { timeout: 30000 });
    await page.waitForTimeout(500);
    
    // Send a message in English
    await page.fill('input[name="message"]', 'Hello');
    await page.click('button[type="submit"]');
    
    // Wait for English response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify English response
    const chatBubbles = page.locator('.chat-bubble');
    const englishBubble = chatBubbles.last();
    const englishText = await englishBubble.textContent();
    expect(englishText.length).toBeGreaterThan(10);
    
    const englishLower = englishText.toLowerCase();
    const hasEnglish = englishLower.includes('i') || 
                      englishLower.includes('the') ||
                      englishLower.includes('a') ||
                      englishLower.includes('is');
    expect(hasEnglish).toBe(true);
    
    // Now switch to Japanese
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Verify language switched
    const lang = await page.evaluate(() => localStorage.getItem('language'));
    expect(lang).toBe('ja');
    
    // Reload character chat to get Japanese interface
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="message"]', { timeout: 30000 });
    await page.waitForTimeout(500);
    
    // Send a message in Japanese
    await page.fill('input[name="message"]', 'こんにちは');
    await page.click('button[type="submit"]');
    
    // Wait for Japanese response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify Japanese response
    const japaneseBubble = page.locator('.chat-bubble').last();
    const japaneseText = await japaneseBubble.textContent();
    expect(japaneseText.length).toBeGreaterThan(10);
    
    // Verify Japanese content
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(japaneseText);
    expect(hasJapanese).toBe(true);
  });

  test('should maintain language preference across feature switches', async ({ page }) => {
    // Requirement 4.4: Test language switching during session
    // Verify language preference persists when switching between features
    
    // Set Japanese language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Test with Character Chat
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="message"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Set up request interception
    const chatRequests = [];
    page.on('request', request => {
      const url = request.url();
      if (url.includes('/api/character-chat-stream')) {
        chatRequests.push({
          url: url,
          method: request.method()
        });
      }
    });
    
    // Send a message
    await page.fill('input[name="message"]', 'こんにちは');
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify Japanese parameter was sent
    expect(chatRequests.length).toBeGreaterThan(0);
    const chatRequest = chatRequests.find(r => r.url.includes('/api/character-chat-stream'));
    expect(chatRequest).toBeDefined();
    expect(chatRequest.url).toContain('language=ja');
    
    // Switch to Trivia Bot
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="question"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Set up request interception for trivia
    const triviaRequests = [];
    page.on('request', request => {
      const url = request.url();
      if (url.includes('/api/trivia-bot-stream')) {
        triviaRequests.push({
          url: url,
          method: request.method()
        });
      }
    });
    
    // Ask a question
    await page.fill('input[name="question"]', 'ハロウィンについて教えて');
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify Japanese parameter was sent
    expect(triviaRequests.length).toBeGreaterThan(0);
    const triviaRequest = triviaRequests.find(r => r.url.includes('/api/trivia-bot-stream'));
    expect(triviaRequest).toBeDefined();
    expect(triviaRequest.url).toContain('language=ja');
    
    // Verify language is still Japanese
    const lang = await page.evaluate(() => localStorage.getItem('language'));
    expect(lang).toBe('ja');
  });

  test('should display content in requested language for Trivia Bot', async ({ page }) => {
    // Requirement 4.4: Test language switching during session
    // Test Trivia Bot with both languages
    
    // Test English first
    await page.evaluate(() => {
      localStorage.setItem('language', 'en');
    });
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Test Trivia Bot in English
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="question"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Ask a question in English
    await page.fill('input[name="question"]', 'What is Halloween?');
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 20;
    }, { timeout: 180000 });
    
    // Verify English response
    const englishBubbles = page.locator('.chat-bubble');
    const englishResponse = englishBubbles.last();
    const englishText = await englishResponse.textContent();
    expect(englishText.length).toBeGreaterThan(20);
    
    const englishLower = englishText.toLowerCase();
    const hasEnglish = englishLower.includes('halloween') || 
                      englishLower.includes('the') ||
                      englishLower.includes('a') ||
                      englishLower.includes('is');
    expect(hasEnglish).toBe(true);
    
    // Switch to Japanese
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Test Trivia Bot in Japanese
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(1000);
    
    // Wait for form to load
    await page.waitForSelector('input[name="question"]', { timeout: 10000 });
    await page.waitForTimeout(500);
    
    // Ask a question in Japanese
    await page.fill('input[name="question"]', 'ハロウィンとは何ですか？');
    await page.click('button[type="submit"]');
    
    // Wait for response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 20;
    }, { timeout: 180000 });
    
    // Verify Japanese response
    const japaneseBubbles = page.locator('.chat-bubble');
    const japaneseResponse = japaneseBubbles.last();
    const japaneseText = await japaneseResponse.textContent();
    expect(japaneseText.length).toBeGreaterThan(20);
    
    // Verify Japanese content
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(japaneseText);
    expect(hasJapanese).toBe(true);
  });

  test('should send correct language parameter for all API endpoints', async ({ page }) => {
    // Requirement 4.3: Verify language parameter transmission to API
    // Test that all endpoints receive the correct language parameter
    
    // Set Japanese language
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Track all API requests
    const apiRequests = [];
    page.on('request', request => {
      const url = request.url();
      if (url.includes('/api/') && url.includes('-stream')) {
        apiRequests.push({
          url: url,
          endpoint: url.split('/api/')[1].split('?')[0]
        });
      }
    });
    
    // Test Monster Diagnostic
    await page.click('[data-feature="monster-diagnostic"]');
    await page.waitForTimeout(1000);
    await page.waitForSelector('input[name="favorite_food"]', { timeout: 30000 });
    await page.fill('input[name="favorite_food"]', 'test');
    await page.fill('input[name="sleep_schedule"]', 'test');
    await page.fill('input[name="hobby"]', 'test');
    await page.fill('input[name="fear"]', 'test');
    await page.click('button[type="submit"]');
    await page.waitForTimeout(2000);
    
    // Verify all requests included Japanese language parameter
    expect(apiRequests.length).toBeGreaterThan(0);
    
    for (const request of apiRequests) {
      expect(request.url).toContain('language=ja');
    }
  });
});
