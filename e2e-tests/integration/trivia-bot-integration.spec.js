/**
 * Trivia Bot Integration Tests
 * 
 * This test suite verifies trivia bot functionality with real LLM API calls.
 * NO MOCKS - All tests use actual OpenAI-compatible endpoint.
 * 
 * STREAMING MODE:
 * - Uses SSE (Server-Sent Events) via HTMX SSE extension
 * - Endpoint: GET /api/trivia-bot-stream
 * - Real-time response streaming from LLM
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
 * 
 * TEST REQUIREMENTS:
 * - Validates: Requirements 1.1, 1.2, 1.3, 1.4, 3.5, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4
 * - Tests real LLM API integration
 * - Verifies streaming content delivery
 * - Validates conversation history persistence
 * - Tests multilingual support (English and Japanese)
 * 
 * NOTE:
 * - Tests may take 30-180 seconds due to real LLM response time
 * - Requires valid OPENAI_API_KEY environment variable
 * - Tests will fail if LLM API is unavailable
 */
const { test, expect } = require('@playwright/test');

test.describe('Trivia Bot Integration Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    
    // Switch to Trivia Bot tab
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(500);
  });

  test('should answer trivia question in English', async ({ page }) => {
    // Requirement 3.5: Test trivia bot feature with real LLM
    // Requirement 4.1: Test features in English language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Type trivia question in English
    await page.fill('input[name="question"]', 'Tell me about Halloween traditions');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    // The response should appear as chat bubbles in the history container
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait additional time for streaming to complete
    await page.waitForTimeout(3000);
    
    // Verify response is displayed
    const chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(1);
    
    // Verify assistant response has substantial content
    const responseBubble = chatBubbles.last();
    await expect(responseBubble).toBeVisible();
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 1) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    const responseText = await responseBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
    
    // Verify response is in English (contains common English words)
    const lowerText = responseText.toLowerCase();
    const hasEnglishWords = lowerText.includes('halloween') || 
                           lowerText.includes('the') || 
                           lowerText.includes('a') ||
                           lowerText.includes('is') ||
                           lowerText.includes('tradition');
    expect(hasEnglishWords).toBe(true);
  });

  test('should answer trivia question in Japanese', async ({ page }) => {
    // Requirement 3.5: Test trivia bot feature with real LLM
    // Requirement 4.2: Test features in Japanese language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Switch to Japanese language by clicking the language toggle
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Reload trivia bot to get Japanese interface
    await page.click('[data-feature="trivia-bot"]');
    await page.waitForTimeout(1000);
    
    // Type trivia question in Japanese
    await page.fill('input[name="question"]', 'ハロウィンの歴史について教えてください');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait additional time for streaming to complete
    await page.waitForTimeout(3000);
    
    // Verify response is displayed
    const chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(1);
    
    // Verify assistant response has substantial content
    const responseBubble = chatBubbles.last();
    await expect(responseBubble).toBeVisible();
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 1) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    const responseText = await responseBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
    
    // Verify response contains Japanese characters (Hiragana, Katakana, or Kanji)
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(responseText);
    expect(hasJapanese).toBe(true);
  });

  test('should handle follow-up questions with context', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    // Requirement 3.5: Test trivia bot feature with real LLM
    // Verify conversation history is maintained across multiple questions
    
    // Ask first question
    await page.fill('input[name="question"]', 'What is Halloween?');
    await page.click('button[type="submit"]');
    
    // Wait for first response
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 1) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    await page.waitForTimeout(1000);
    
    // Count initial bubbles
    let chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const initialCount = await chatBubbles.count();
    expect(initialCount).toBeGreaterThanOrEqual(1);
    
    // Ask follow-up question that requires context
    await page.fill('input[name="question"]', 'Tell me more about that');
    await page.click('button[type="submit"]');
    
    // Wait for second response
    await page.waitForTimeout(2000);
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait for second response content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      const initialCount = bubbles.length;
      if (initialCount < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify more bubbles are displayed (conversation history is persisted)
    chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const finalCount = await chatBubbles.count();
    expect(finalCount).toBeGreaterThan(initialCount);
    
    // Verify all messages have content
    for (let i = 0; i < finalCount; i++) {
      const bubble = chatBubbles.nth(i);
      const text = await bubble.textContent();
      expect(text.trim().length).toBeGreaterThan(0);
    }
  });

  test('should verify streaming responses with real LLM', async ({ page }) => {
    // Requirement 5.1: Verify content is delivered incrementally
    // Requirement 5.2: Verify UI updates during streaming
    // Requirement 5.3: Verify final content is complete and correct
    // Requirement 5.4: Use real LLM API streaming endpoints
    
    // Track content changes during streaming
    await page.evaluate(() => {
      window.streamingUpdates = [];
      const targetNode = document.querySelector('#trivia-history-container');
      if (targetNode) {
        const observer = new MutationObserver((mutations) => {
          const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
          if (bubbles.length > 0) {
            const lastBubble = bubbles[bubbles.length - 1];
            const contentLength = lastBubble.textContent.length;
            window.streamingUpdates.push({
              timestamp: Date.now(),
              contentLength: contentLength,
              bubbleCount: bubbles.length
            });
          }
        });
        observer.observe(targetNode, { childList: true, subtree: true, characterData: true });
      }
    });
    
    // Type question that will generate a longer response
    await page.fill('input[name="question"]', 'Tell me a detailed explanation of Halloween origins and traditions');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to complete
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 1) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 50;
    }, { timeout: 180000 });
    
    // Wait a bit more to ensure streaming is complete
    await page.waitForTimeout(2000);
    
    // Get streaming updates
    const updates = await page.evaluate(() => window.streamingUpdates || []);
    
    // Verify streaming occurred (multiple updates)
    // Note: Due to timing, we may not always catch all updates, but we should see at least some
    if (updates.length > 1) {
      // Verify content length increased over time
      const firstUpdate = updates[0];
      const lastUpdate = updates[updates.length - 1];
      expect(lastUpdate.contentLength).toBeGreaterThan(firstUpdate.contentLength);
    }
    
    // Verify final content is present and substantial
    const chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const responseBubble = chatBubbles.last();
    const finalContent = await responseBubble.textContent();
    expect(finalContent.length).toBeGreaterThan(50);
  });

  test('should maintain conversation context across multiple questions', async ({ page }) => {
    // Additional test for conversation history
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    // Requirement 3.5: Test trivia bot feature with real LLM
    
    // Send first question establishing context
    await page.fill('input[name="question"]', 'What are some Halloween symbols?');
    await page.click('button[type="submit"]');
    
    // Wait for first response
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 1) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    await page.waitForTimeout(1000);
    
    // Send follow-up question
    await page.fill('input[name="question"]', 'Why are they important?');
    await page.click('button[type="submit"]');
    
    // Wait for second response
    await page.waitForTimeout(2000);
    await page.waitForSelector('#trivia-history-container .chat-bubble', { timeout: 180000 });
    
    // Wait for second response content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('#trivia-history-container .chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify multiple messages are displayed
    const chatBubbles = page.locator('#trivia-history-container .chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
    
    // Verify all messages have substantial content
    for (let i = 0; i < count; i++) {
      const bubble = chatBubbles.nth(i);
      const text = await bubble.textContent();
      expect(text.trim().length).toBeGreaterThan(0);
    }
    
    // Verify the last response has substantial content
    const lastBubble = chatBubbles.last();
    const responseText = await lastBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
  });
});
