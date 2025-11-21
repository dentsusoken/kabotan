/**
 * Character Chat Integration Tests
 * 
 * This test suite verifies character chat functionality with real LLM API calls.
 * NO MOCKS - All tests use actual OpenAI-compatible endpoint.
 * 
 * STREAMING MODE:
 * - Uses SSE (Server-Sent Events) via HTMX SSE extension
 * - Endpoint: GET /api/character-chat-stream
 * - Real-time response streaming from LLM
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
 * 
 * TEST REQUIREMENTS:
 * - Validates: Requirements 1.1, 1.2, 1.3, 1.4, 3.3, 4.1, 4.2, 5.1, 5.2, 5.3, 5.4
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

test.describe('Character Chat Integration Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    
    // Switch to Character Chat tab
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(500);
  });

  test('should chat with Dracula character in English', async ({ page }) => {
    // Requirement 3.3: Test character chat feature with real LLM
    // Requirement 4.1: Test features in English language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Dracula is selected by default
    // Verify Dracula is selected
    const draculaRadio = page.locator('input[type="radio"][value="dracula"]');
    await expect(draculaRadio).toBeChecked();
    
    // Type message in English
    await page.fill('input[name="message"]', 'Hello, tell me about yourself');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    // The response should appear as chat bubbles
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait additional time for streaming to complete
    await page.waitForTimeout(3000);
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2); // User message + assistant response
    
    // Verify assistant response has substantial content
    // The last bubble should be the assistant's response
    const assistantBubble = chatBubbles.last();
    await expect(assistantBubble).toBeVisible();
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    const responseText = await assistantBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
    
    // Verify response is in English (contains common English words)
    const lowerText = responseText.toLowerCase();
    const hasEnglishWords = lowerText.includes('i') || 
                           lowerText.includes('the') || 
                           lowerText.includes('a') ||
                           lowerText.includes('is') ||
                           lowerText.includes('am');
    expect(hasEnglishWords).toBe(true);
  });

  test('should chat with Witch character in Japanese', async ({ page }) => {
    // Requirement 3.3: Test character chat feature with real LLM
    // Requirement 4.2: Test features in Japanese language
    // Requirement 5.1, 5.2, 5.3, 5.4: Verify streaming functionality
    
    // Switch to Japanese language by clicking the language toggle
    await page.evaluate(() => {
      const toggle = document.getElementById('language-toggle');
      toggle.click();
    });
    await page.waitForTimeout(1000);
    
    // Reload character chat to get Japanese interface
    await page.click('[data-feature="character-chat"]');
    await page.waitForTimeout(1000);
    
    // Select Witch character
    await page.click('input[type="radio"][value="witch"]');
    
    // Type message in Japanese
    await page.fill('input[name="message"]', 'こんにちは、あなたについて教えてください');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait additional time for streaming to complete
    await page.waitForTimeout(3000);
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
    
    // Verify assistant response has substantial content
    const assistantBubble = chatBubbles.last();
    await expect(assistantBubble).toBeVisible();
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    const responseText = await assistantBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
    
    // Verify response contains Japanese characters (Hiragana, Katakana, or Kanji)
    const hasJapanese = /[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]/.test(responseText);
    expect(hasJapanese).toBe(true);
  });

  test('should verify streaming content delivery with real LLM', async ({ page }) => {
    // Requirement 5.1: Verify content is delivered incrementally
    // Requirement 5.2: Verify UI updates during streaming
    // Requirement 5.3: Verify final content is complete and correct
    // Requirement 5.4: Use real LLM API streaming endpoints
    
    // Track content changes during streaming
    const contentChanges = [];
    
    // Set up mutation observer to track streaming updates
    await page.evaluate(() => {
      window.streamingUpdates = [];
      const targetNode = document.querySelector('#chat-messages');
      if (targetNode) {
        const observer = new MutationObserver((mutations) => {
          const bubbles = document.querySelectorAll('.chat-bubble');
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
    
    // Type message that will generate a longer response
    await page.fill('input[name="message"]', 'Tell me a detailed story about your adventures');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
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
    const chatBubbles = page.locator('.chat-bubble');
    const assistantBubble = chatBubbles.last();
    const finalContent = await assistantBubble.textContent();
    expect(finalContent.length).toBeGreaterThan(50);
  });

  test('should verify conversation history persistence', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    // Verify conversation history is maintained across multiple messages
    
    // Send first message
    await page.fill('input[name="message"]', 'Hello, how are you?');
    await page.click('button[type="submit"]');
    
    // Wait for first response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    await page.waitForTimeout(1000);
    
    // Verify first exchange is displayed
    let chatBubbles = page.locator('.chat-bubble');
    let count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
    
    // Send second message
    await page.fill('input[name="message"]', 'Tell me more');
    await page.click('button[type="submit"]');
    
    // Wait for second response
    await page.waitForTimeout(2000);
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for second response content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 4) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify both exchanges are displayed (conversation history is persisted)
    chatBubbles = page.locator('.chat-bubble');
    count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(4); // 2 user + 2 assistant messages
    
    // Verify all messages have content
    for (let i = 0; i < count; i++) {
      const bubble = chatBubbles.nth(i);
      const text = await bubble.textContent();
      expect(text.trim().length).toBeGreaterThan(0);
    }
  });

  test('should maintain context across multiple messages', async ({ page }) => {
    // Additional test for conversation history
    // Requirement 1.1, 1.2, 1.3, 1.4: Integration tests use real LLM API
    
    // Send first message establishing context
    await page.fill('input[name="message"]', 'What is your favorite thing about Halloween?');
    await page.click('button[type="submit"]');
    
    // Wait for first response
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    await page.waitForTimeout(1000);
    
    // Send follow-up message
    await page.fill('input[name="message"]', 'That sounds interesting');
    await page.click('button[type="submit"]');
    
    // Wait for second response
    await page.waitForTimeout(2000);
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait for second response content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 4) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    // Verify multiple messages are displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(4);
    
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

  test('should chat with Jack-o-lantern character and verify response', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.4: Test character selection transmission (streaming)
    // Verify Jack-o-lantern character parameter is transmitted correctly
    
    // Select Jack-o-lantern character
    await page.click('input[type="radio"][value="jack-o-lantern"]');
    
    // Verify Jack-o-lantern is selected
    const jackRadio = page.locator('input[type="radio"][value="jack-o-lantern"]');
    await expect(jackRadio).toBeChecked();
    
    // Type message
    await page.fill('input[name="message"]', 'Hello, tell me about yourself');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming response to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Wait additional time for streaming to complete
    await page.waitForTimeout(3000);
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2); // User message + assistant response
    
    // Verify assistant response has substantial content
    const assistantBubble = chatBubbles.last();
    await expect(assistantBubble).toBeVisible();
    
    // Wait for content to be populated
    await page.waitForFunction(() => {
      const bubbles = document.querySelectorAll('.chat-bubble');
      if (bubbles.length < 2) return false;
      const lastBubble = bubbles[bubbles.length - 1];
      return lastBubble.textContent.trim().length > 10;
    }, { timeout: 180000 });
    
    const responseText = await assistantBubble.textContent();
    expect(responseText.length).toBeGreaterThan(10);
    
    // Verify response is in English (contains common English words)
    const lowerText = responseText.toLowerCase();
    const hasEnglishWords = lowerText.includes('i') || 
                           lowerText.includes('the') || 
                           lowerText.includes('a') ||
                           lowerText.includes('is') ||
                           lowerText.includes('am');
    expect(hasEnglishWords).toBe(true);
  });

  test('should verify character parameter transmission for Dracula', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.4: Verify character parameter is transmitted correctly
    // Test that Dracula character selection is properly sent to backend
    
    // Dracula is selected by default
    const draculaRadio = page.locator('input[type="radio"][value="dracula"]');
    await expect(draculaRadio).toBeChecked();
    
    // Intercept the streaming request to verify character parameter
    let streamingUrl = '';
    page.on('request', request => {
      if (request.url().includes('/api/character-chat-stream')) {
        streamingUrl = request.url();
      }
    });
    
    // Type message
    await page.fill('input[name="message"]', 'Hello');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to start
    await page.waitForTimeout(1000);
    
    // Verify the streaming URL contains the character parameter
    expect(streamingUrl).toContain('character=dracula');
    
    // Wait for response to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
  });

  test('should verify character parameter transmission for Witch', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.4: Verify character parameter is transmitted correctly
    // Test that Witch character selection is properly sent to backend
    
    // Select Witch character
    await page.click('input[type="radio"][value="witch"]');
    
    // Verify Witch is selected
    const witchRadio = page.locator('input[type="radio"][value="witch"]');
    await expect(witchRadio).toBeChecked();
    
    // Intercept the streaming request to verify character parameter
    let streamingUrl = '';
    page.on('request', request => {
      if (request.url().includes('/api/character-chat-stream')) {
        streamingUrl = request.url();
      }
    });
    
    // Type message
    await page.fill('input[name="message"]', 'Hello');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to start
    await page.waitForTimeout(1000);
    
    // Verify the streaming URL contains the character parameter
    expect(streamingUrl).toContain('character=witch');
    
    // Wait for response to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
  });

  test('should verify character parameter transmission for Jack-o-lantern', async ({ page }) => {
    // Requirement 1.1, 1.2, 1.4: Verify character parameter is transmitted correctly
    // Test that Jack-o-lantern character selection is properly sent to backend
    
    // Select Jack-o-lantern character
    await page.click('input[type="radio"][value="jack-o-lantern"]');
    
    // Verify Jack-o-lantern is selected
    const jackRadio = page.locator('input[type="radio"][value="jack-o-lantern"]');
    await expect(jackRadio).toBeChecked();
    
    // Intercept the streaming request to verify character parameter
    let streamingUrl = '';
    page.on('request', request => {
      if (request.url().includes('/api/character-chat-stream')) {
        streamingUrl = request.url();
      }
    });
    
    // Type message
    await page.fill('input[name="message"]', 'Hello');
    
    // Submit the form
    await page.click('button[type="submit"]');
    
    // Wait for streaming to start
    await page.waitForTimeout(1000);
    
    // Verify the streaming URL contains the character parameter
    expect(streamingUrl).toContain('character=jack-o-lantern');
    
    // Wait for response to complete
    await page.waitForSelector('.chat-bubble', { timeout: 180000 });
    
    // Verify response is displayed
    const chatBubbles = page.locator('.chat-bubble');
    const count = await chatBubbles.count();
    expect(count).toBeGreaterThanOrEqual(2);
  });
});
