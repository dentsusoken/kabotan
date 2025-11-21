/**
 * Mock Response Verification Script
 * 
 * This script compares mock responses with real API responses to ensure
 * that mocks accurately represent the backend behavior.
 * 
 * Usage:
 *   node e2e-tests/scripts/verify-mock-responses.js
 * 
 * Requirements:
 *   - Application must be running on http://localhost:5000
 *   - OPENAI_API_KEY must be set for real API calls
 */

const { chromium } = require('@playwright/test');
const { createMockNonStreamingResponse, MOCK_RESPONSES } = require('../helpers/mock-llm-api');

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Extract HTML structure from response (ignoring content)
 */
function extractStructure(html) {
  // Remove text content but keep HTML structure
  return html
    .replace(/>([^<]+)</g, '><')  // Remove text between tags
    .replace(/\s+/g, ' ')          // Normalize whitespace
    .trim();
}

/**
 * Compare HTML structures
 */
function compareStructures(mockHtml, realHtml, feature) {
  const mockStructure = extractStructure(mockHtml);
  const realStructure = extractStructure(realHtml);
  
  const match = mockStructure === realStructure;
  
  return {
    match,
    mockStructure,
    realStructure,
    feature,
  };
}

/**
 * Verify a single feature's mock response
 */
async function verifyFeature(page, feature, options = {}) {
  console.log(`\n${colors.cyan}Testing ${feature}...${colors.reset}`);
  
  const { endpoint, lang = 'en', character = null } = options;
  
  // Generate mock response
  const mockResponse = createMockNonStreamingResponse(feature, lang, { character });
  
  // Make real API call
  let realResponse = null;
  try {
    const response = await page.request.post(`http://localhost:5000${endpoint}`, {
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      form: options.requestBody || {},
      timeout: 180000,
    });
    
    if (response.ok()) {
      realResponse = await response.text();
    } else {
      console.log(`${colors.red}✗ API call failed with status ${response.status()}${colors.reset}`);
      return { success: false, feature, error: `HTTP ${response.status()}` };
    }
  } catch (error) {
    console.log(`${colors.red}✗ API call error: ${error.message}${colors.reset}`);
    return { success: false, feature, error: error.message };
  }
  
  // Compare structures
  const comparison = compareStructures(mockResponse, realResponse, feature);
  
  if (comparison.match) {
    console.log(`${colors.green}✓ Structure matches${colors.reset}`);
    return { success: true, feature };
  } else {
    console.log(`${colors.red}✗ Structure mismatch${colors.reset}`);
    console.log(`${colors.yellow}Mock structure:${colors.reset}`);
    console.log(comparison.mockStructure);
    console.log(`${colors.yellow}Real structure:${colors.reset}`);
    console.log(comparison.realStructure);
    return { 
      success: false, 
      feature, 
      mockStructure: comparison.mockStructure,
      realStructure: comparison.realStructure,
    };
  }
}

/**
 * Main verification function
 */
async function verifyAllMocks() {
  console.log(`${colors.blue}=== Mock Response Verification ===${colors.reset}`);
  console.log('Comparing mock responses with real API responses...\n');
  
  const browser = await chromium.launch();
  const context = await browser.newContext();
  const page = await context.newPage();
  
  const results = [];
  
  // Test Monster Diagnostic
  results.push(await verifyFeature(page, 'monster-diagnostic', {
    endpoint: '/api/monster-diagnostic',
    lang: 'en',
    requestBody: {
      'favorite-food': 'rare steak',
      'sleep-schedule': 'night',
      'hobby': 'reading',
      'fear': 'sunlight',
      'language': 'en',
    },
  }));
  
  // Test Story Generator
  results.push(await verifyFeature(page, 'story-generator', {
    endpoint: '/api/story-generator',
    lang: 'en',
    requestBody: {
      name: 'wizard',
      theme: 'adventure',
      style: 'classic',
      language: 'en',
    },
  }));
  
  // Test Spell Generator
  results.push(await verifyFeature(page, 'spell-generator', {
    endpoint: '/api/spell-generator',
    lang: 'en',
    requestBody: {
      language: 'en',
    },
  }));
  
  // Test Character Chat - Dracula
  results.push(await verifyFeature(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'dracula',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'dracula',
      language: 'en',
    },
  }));
  
  // Test Character Chat - Witch
  results.push(await verifyFeature(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'witch',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'witch',
      language: 'en',
    },
  }));
  
  // Test Character Chat - Jack
  results.push(await verifyFeature(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'jack',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'jack',
      language: 'en',
    },
  }));
  
  // Test Trivia Bot
  results.push(await verifyFeature(page, 'trivia-bot', {
    endpoint: '/api/trivia-bot',
    lang: 'en',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Tell me about Halloween' }]),
      language: 'en',
    },
  }));
  
  // Test Japanese language support
  console.log(`\n${colors.blue}=== Testing Japanese Language Support ===${colors.reset}`);
  
  // Test Monster Diagnostic (Japanese)
  results.push(await verifyFeature(page, 'monster-diagnostic', {
    endpoint: '/api/monster-diagnostic',
    lang: 'ja',
    requestBody: {
      'favorite-food': 'レアステーキ',
      'sleep-schedule': '夜',
      'hobby': '読書',
      'fear': '日光',
      'language': 'ja',
    },
  }));
  
  // Test Story Generator (Japanese)
  results.push(await verifyFeature(page, 'story-generator', {
    endpoint: '/api/story-generator',
    lang: 'ja',
    requestBody: {
      name: '魔法使い',
      theme: '冒険',
      style: 'classic',
      language: 'ja',
    },
  }));
  
  await browser.close();
  
  // Print summary
  console.log(`\n${colors.blue}=== Verification Summary ===${colors.reset}`);
  
  const successful = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  
  console.log(`${colors.green}✓ Passed: ${successful}${colors.reset}`);
  console.log(`${colors.red}✗ Failed: ${failed}${colors.reset}`);
  
  if (failed > 0) {
    console.log(`\n${colors.yellow}Failed features:${colors.reset}`);
    results.filter(r => !r.success).forEach(r => {
      console.log(`  - ${r.feature}: ${r.error || 'Structure mismatch'}`);
    });
  }
  
  // Exit with appropriate code
  process.exit(failed > 0 ? 1 : 0);
}

// Run verification
verifyAllMocks().catch(error => {
  console.error(`${colors.red}Fatal error: ${error.message}${colors.reset}`);
  console.error(error.stack);
  process.exit(1);
});
