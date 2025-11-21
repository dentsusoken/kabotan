/**
 * Mock helpers for LLM API responses in E2E tests
 *
 * This module provides utilities to mock LLM API responses without requiring
 * actual communication with the LLM server, significantly speeding up E2E tests.
 */

/**
 * Mock response templates for different features
 */
const MOCK_RESPONSES = {
  'character-chat-dracula': 'Good evening, mortal. I am Count Dracula, master of the night. What brings you to my castle?',
  'character-chat-witch': 'Greetings, dear visitor! I am the Witch of the Forest. How may I assist you with my magical knowledge?',
  'character-chat-jack': 'Hello there! I am Jack the Pumpkin. Happy Halloween! What would you like to talk about?',
  'monster-diagnostic': 'Based on your answers, you are most like a Vampire! You prefer the night, enjoy mysterious activities, and have a refined taste.',
  'story-generator': 'Once upon a Halloween night, in a town filled with mystery and magic, there lived a brave adventurer. Their journey was about to begin...',
  'spell-generator': 'Spell: Moonlight Shield\nMeaning: Creates a protective barrier of silvery moonlight that deflects dark magic. Incantation: "Luna protectus, shield me bright!"',
  'trivia-bot': 'That is an interesting question! Here is what I know: Halloween originated from the ancient Celtic festival of Samhain...',
};

/**
 * Create a mock non-streaming response
 *
 * @param {string} feature - The feature name (e.g., 'monster-diagnostic', 'character-chat')
 * @param {string} lang - Language code ('en' or 'ja')
 * @param {Object} options - Additional options
 * @param {string} options.character - Character name for character-chat (optional)
 * @param {string} options.customResponse - Custom response text (optional)
 * @returns {string} HTML response
 */
function createMockNonStreamingResponse(feature, lang, options = {}) {
  const { character, customResponse } = options;

  let responseText = customResponse;
  if (!responseText) {
    if (feature === 'character-chat' && character) {
      responseText = MOCK_RESPONSES[`character-chat-${character}`] || MOCK_RESPONSES['character-chat-dracula'];
    } else {
      responseText = MOCK_RESPONSES[feature] || 'This is a mock response for testing purposes.';
    }
  }

  // Format response based on feature (matching backend response-formatting.lisp)
  if (feature === 'character-chat') {
    // Match format-character-chat-response-with-history from backend
    // Returns both user message and assistant response
    const userLabel = lang === 'ja' ? 'メッセージ' : 'Message';
    const userMessage = 'Hello!'; // Default mock message
    
    const characterLabels = {
      'dracula': lang === 'ja' ? 'ドラキュラ伯爵' : 'Count Dracula',
      'witch': lang === 'ja' ? '魔女' : 'Witch',
      'jack': lang === 'ja' ? 'ジャック・オー・ランタン' : 'Jack-o-Lantern',
      'jack-o-lantern': lang === 'ja' ? 'ジャック・オー・ランタン' : 'Jack-o-Lantern',
    };
    const characterLabel = characterLabels[character] || 'Character';

    return `<div class="chat chat-end">
  <div class="chat-header">${escapeHtml(userLabel)}</div>
  <div class="chat-bubble chat-bubble-primary">${escapeHtml(userMessage)}</div>
</div>
<div class="chat chat-start">
  <div class="chat-header">${escapeHtml(characterLabel)}</div>
  <div class="chat-bubble chat-bubble-secondary">${escapeHtml(responseText)}</div>
</div>`;
  } else if (feature === 'trivia-bot') {
    // Match format-trivia-response-with-history from backend
    // Returns both user question and assistant answer
    const userLabel = lang === 'ja' ? '質問' : 'Question';
    const userQuestion = 'Tell me about Halloween origins'; // Default mock question
    
    return `<div class="chat chat-end">
  <div class="chat-header">${escapeHtml(userLabel)}</div>
  <div class="chat-bubble chat-bubble-primary">${escapeHtml(userQuestion)}</div>
</div>
<div class="chat chat-start">
  <div class="chat-header">🎓 Trivia Bot</div>
  <div class="chat-bubble chat-bubble-secondary">${escapeHtml(responseText)}</div>
</div>`;
  } else if (feature === 'monster-diagnostic') {
    // Match format-monster-diagnostic-response from backend
    const title = lang === 'ja' ? '診断結果' : 'Diagnosis Result';
    return `<div class="alert alert-success">
  <h3 class="font-bold text-lg mb-2">${title}</h3>
  <div class="whitespace-pre-wrap">${escapeHtml(responseText)}</div>
</div>`;
  } else if (feature === 'story-generator') {
    // Match format-story-response from backend
    const title = lang === 'ja' ? 'あなたのストーリー' : 'Your Story';
    return `<div class="bg-base-300 rounded-lg p-4">
  <h3 class="font-bold text-lg mb-2">${title}</h3>
  <div class="whitespace-pre-wrap">${escapeHtml(responseText)}</div>
</div>`;
  } else if (feature === 'spell-generator') {
    // Match format-spell-generator-response from backend
    // Parse "Spell:" and "Meaning:" from response text
    const spellMatch = responseText.match(/Spell:\s*([^\n]+)/);
    const meaningMatch = responseText.match(/Meaning:\s*([\s\S]+)/);
    const spellText = spellMatch ? spellMatch[1].trim() : '';
    const meaningText = meaningMatch ? meaningMatch[1].trim() : '';

    return `<div class="text-center">
  <div class="spell-phrase">${escapeHtml(spellText)}</div>
  <div class="spell-explanation mt-6">${escapeHtml(meaningText)}</div>
</div>`;
  } else {
    // Fallback for unknown features
    return `<div class="alert alert-info shadow-lg">
      <div>${escapeHtml(responseText)}</div>
    </div>`;
  }
}

/**
 * Create SSE (Server-Sent Events) format streaming chunks
 *
 * @param {string} text - Full text to stream
 * @param {number} chunkSize - Number of characters per chunk
 * @returns {Array<string>} Array of SSE-formatted chunks
 */
function createStreamingChunks(text, chunkSize = 5) {
  const chunks = [];

  for (let i = 0; i < text.length; i += chunkSize) {
    const chunk = text.substring(i, i + chunkSize);
    // SSE format matching backend: data: {"chunk":"...","type":"content"}
    chunks.push(`data: ${JSON.stringify({
      chunk: chunk,
      type: "content"
    })}\n\n`);
  }

  // Add completion marker matching backend format
  chunks.push(`data: ${JSON.stringify({
    done: true,
    type: "done"
  })}\n\n`);

  return chunks;
}

/**
 * Setup mock for non-streaming API endpoint
 *
 * @param {Page} page - Playwright page object
 * @param {string} endpoint - API endpoint (e.g., '/api/monster-diagnostic')
 * @param {Object} options - Mock options
 * @param {string} options.feature - Feature name
 * @param {string} options.lang - Language code
 * @param {string} options.character - Character name (for character-chat)
 * @param {string} options.customResponse - Custom response text
 * @param {number} options.delay - Response delay in ms (default: 100)
 * @param {number} options.status - HTTP status code (default: 200)
 */
async function mockNonStreamingEndpoint(page, endpoint, options = {}) {
  const {
    feature,
    lang = 'en',
    character = null,
    customResponse = null,
    delay = 100,
    status = 200,
  } = options;

  await page.route(endpoint, async (route) => {
    const url = route.request().url();
    const method = route.request().method();
    console.log(`[MOCK HIT] Non-streaming endpoint: ${method} ${url}`);
    console.log(`[MOCK HIT] Feature: ${feature}, Delay: ${delay}ms`);
    
    // Simulate network delay
    await new Promise(resolve => setTimeout(resolve, delay));

    const response = createMockNonStreamingResponse(feature, lang, {
      character,
      customResponse,
    });

    console.log(`[MOCK RESPONSE] Sending ${response.length} bytes, status ${status}`);
    await route.fulfill({
      status,
      contentType: 'text/html; charset=utf-8',
      body: response,
    });
  });
}

/**
 * Setup mock for streaming API endpoint
 *
 * @param {Page} page - Playwright page object
 * @param {string} endpoint - API endpoint (e.g., '/api/character-chat-stream')
 * @param {Object} options - Mock options
 * @param {string} options.feature - Feature name
 * @param {string} options.lang - Language code
 * @param {string} options.character - Character name (for character-chat)
 * @param {string} options.customResponse - Custom response text
 * @param {number} options.chunkSize - Characters per chunk (default: 5)
 * @param {number} options.chunkDelay - Delay between chunks in ms (default: 50)
 */
async function mockStreamingEndpoint(page, endpoint, options = {}) {
  const {
    feature,
    lang = 'en',
    character = null,
    customResponse = null,
    chunkSize = 5,
    chunkDelay = 50,
  } = options;

  await page.route(endpoint, async (route) => {
    const url = route.request().url();
    const method = route.request().method();
    console.log(`[MOCK HIT] Streaming endpoint: ${method} ${url}`);
    console.log(`[MOCK HIT] Feature: ${feature}, ChunkSize: ${chunkSize}, ChunkDelay: ${chunkDelay}ms`);
    
    let responseText = customResponse;
    if (!responseText) {
      if (feature === 'character-chat' && character) {
        responseText = MOCK_RESPONSES[`character-chat-${character}`] || MOCK_RESPONSES['character-chat-dracula'];
      } else {
        responseText = MOCK_RESPONSES[feature] || 'This is a mock streaming response for testing purposes.';
      }
    }

    const chunks = createStreamingChunks(responseText, chunkSize);
    console.log(`[MOCK RESPONSE] Streaming ${chunks.length} chunks (${responseText.length} chars total)`);

    // Create SSE response
    let body = '';
    for (const chunk of chunks) {
      body += chunk;
      // Note: We can't actually stream in route.fulfill, so we send all at once
      // but the format is correct for SSE
    }

    console.log(`[MOCK RESPONSE] Sending SSE response, body length: ${body.length} bytes`);
    await route.fulfill({
      status: 200,
      contentType: 'text/event-stream',
      headers: {
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
      },
      body,
    });
  });
}

/**
 * Setup mock for error response
 *
 * @param {Page} page - Playwright page object
 * @param {string} endpoint - API endpoint
 * @param {Object} options - Error options
 * @param {number} options.status - HTTP status code (default: 500)
 * @param {string} options.errorMessage - Error message
 * @param {string} options.lang - Language code
 */
async function mockErrorResponse(page, endpoint, options = {}) {
  const {
    status = 500,
    errorMessage = 'Internal server error',
    lang = 'en',
  } = options;

  await page.route(endpoint, async (route) => {
    await route.fulfill({
      status,
      contentType: 'text/html; charset=utf-8',
      body: `<div class="alert alert-error">${escapeHtml(errorMessage)}</div>`,
    });
  });
}

/**
 * Setup mock to abort request (simulate network failure)
 *
 * @param {Page} page - Playwright page object
 * @param {string} endpoint - API endpoint
 */
async function mockNetworkFailure(page, endpoint) {
  await page.route(endpoint, async (route) => {
    await route.abort();
  });
}

/**
 * Setup comprehensive mocks for all API endpoints
 * This is useful for fast UI tests that don't need real LLM responses
 *
 * @param {Page} page - Playwright page object
 * @param {Object} options - Global mock options
 * @param {string} options.lang - Language code (default: 'en')
 * @param {number} options.delay - Response delay in ms (default: 100)
 */
async function mockAllEndpoints(page, options = {}) {
  const { lang = 'en', delay = 100 } = options;

  // Streaming endpoints (must be registered first to avoid conflicts)
  await mockStreamingEndpoint(page, '**/api/monster-diagnostic-stream*', {
    feature: 'monster-diagnostic',
    lang,
  });

  await mockStreamingEndpoint(page, '**/api/story-generator-stream*', {
    feature: 'story-generator',
    lang,
  });

  await mockStreamingEndpoint(page, '**/api/spell-generator-stream*', {
    feature: 'spell-generator',
    lang,
  });

  await mockStreamingEndpoint(page, '**/api/character-chat-stream*', {
    feature: 'character-chat',
    lang,
  });

  await mockStreamingEndpoint(page, '**/api/trivia-bot-stream*', {
    feature: 'trivia-bot',
    lang,
  });

  // Non-streaming endpoints
  await mockNonStreamingEndpoint(page, '**/api/monster-diagnostic', {
    feature: 'monster-diagnostic',
    lang,
    delay,
  });

  await mockNonStreamingEndpoint(page, '**/api/story-generator', {
    feature: 'story-generator',
    lang,
    delay,
  });

  await mockNonStreamingEndpoint(page, '**/api/spell-generator', {
    feature: 'spell-generator',
    lang,
    delay,
  });

  await mockNonStreamingEndpoint(page, '**/api/character-chat', {
    feature: 'character-chat',
    lang,
    delay,
  });

  await mockNonStreamingEndpoint(page, '**/api/trivia-bot', {
    feature: 'trivia-bot',
    lang,
  });
}

/**
 * Escape HTML special characters
 * Matches the escape-html function from backend (src/utils/response-formatting.lisp)
 *
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 */
function escapeHtml(text) {
  if (!text) return '';
  
  let result = text;
  // Order matters - & must be replaced first
  result = result.replace(/&/g, '&amp;');
  result = result.replace(/</g, '&lt;');
  result = result.replace(/>/g, '&gt;');
  result = result.replace(/"/g, '&quot;');
  result = result.replace(/'/g, '&#39;');
  return result;
}

module.exports = {
  mockNonStreamingEndpoint,
  mockStreamingEndpoint,
  mockErrorResponse,
  mockNetworkFailure,
  mockAllEndpoints,
  createMockNonStreamingResponse,
  createStreamingChunks,
  MOCK_RESPONSES,
};
