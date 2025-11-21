/**
 * HTML Structure Exact Match Verification Script
 * 
 * This script performs detailed verification that mock responses match
 * the exact HTML structure from the backend, including:
 * - Tag structure and nesting
 * - CSS classes
 * - Attributes
 * - HTML escaping
 * 
 * Usage:
 *   node e2e-tests/scripts/verify-html-structure.js
 * 
 * Requirements:
 *   - Application must be running on http://localhost:5000
 *   - OPENAI_API_KEY must be set for real API calls
 */

const { chromium } = require('@playwright/test');
const { createMockNonStreamingResponse } = require('../helpers/mock-llm-api');

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Normalize HTML for comparison
 * - Normalize whitespace between tags
 * - Keep structure intact
 */
function normalizeHtml(html) {
  return html
    .replace(/>\s+</g, '><')  // Remove whitespace between tags
    .replace(/\s+/g, ' ')      // Normalize internal whitespace
    .trim();
}

/**
 * Extract CSS classes from HTML
 */
function extractClasses(html) {
  const classMatches = html.match(/class="([^"]+)"/g) || [];
  return classMatches.map(m => m.match(/class="([^"]+)"/)[1]);
}

/**
 * Extract tag structure
 */
function extractTags(html) {
  const tagMatches = html.match(/<\/?[a-z][^>]*>/gi) || [];
  return tagMatches.map(tag => tag.replace(/\s+/g, ' ').trim());
}

/**
 * Detailed HTML comparison
 */
function compareHtmlDetailed(mockHtml, realHtml, feature) {
  const mockNorm = normalizeHtml(mockHtml);
  const realNorm = normalizeHtml(realHtml);
  
  // Extract components for detailed comparison
  const mockClasses = extractClasses(mockHtml);
  const realClasses = extractClasses(realHtml);
  
  const mockTags = extractTags(mockHtml);
  const realTags = extractTags(realHtml);
  
  const results = {
    feature,
    exactMatch: mockNorm === realNorm,
    classesMatch: JSON.stringify(mockClasses) === JSON.stringify(realClasses),
    tagsMatch: JSON.stringify(mockTags) === JSON.stringify(realTags),
    mockClasses,
    realClasses,
    mockTags,
    realTags,
    mockNorm,
    realNorm,
  };
  
  return results;
}

/**
 * Verify HTML structure for a feature
 */
async function verifyHtmlStructure(page, feature, options = {}) {
  console.log(`\n${colors.cyan}Verifying ${feature} (${options.lang || 'en'})...${colors.reset}`);
  
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
  
  // Detailed comparison
  const comparison = compareHtmlDetailed(mockResponse, realResponse, feature);
  
  // Check exact match
  if (comparison.exactMatch) {
    console.log(`${colors.green}✓ Exact HTML match${colors.reset}`);
    return { success: true, feature, lang };
  }
  
  // If not exact match, show details
  console.log(`${colors.red}✗ HTML mismatch${colors.reset}`);
  
  if (!comparison.classesMatch) {
    console.log(`${colors.yellow}  CSS classes differ:${colors.reset}`);
    console.log(`    Mock: ${JSON.stringify(comparison.mockClasses)}`);
    console.log(`    Real: ${JSON.stringify(comparison.realClasses)}`);
  } else {
    console.log(`${colors.green}  ✓ CSS classes match${colors.reset}`);
  }
  
  if (!comparison.tagsMatch) {
    console.log(`${colors.yellow}  Tag structure differs:${colors.reset}`);
    console.log(`    Mock tags: ${comparison.mockTags.length}`);
    console.log(`    Real tags: ${comparison.realTags.length}`);
  } else {
    console.log(`${colors.green}  ✓ Tag structure matches${colors.reset}`);
  }
  
  // Show normalized HTML for debugging
  console.log(`${colors.yellow}  Mock HTML (normalized):${colors.reset}`);
  console.log(`    ${comparison.mockNorm.substring(0, 200)}...`);
  console.log(`${colors.yellow}  Real HTML (normalized):${colors.reset}`);
  console.log(`    ${comparison.realNorm.substring(0, 200)}...`);
  
  return { 
    success: false, 
    feature, 
    lang,
    comparison,
  };
}

/**
 * Main verification function
 */
async function verifyAll() {
  console.log(`${colors.blue}=== HTML Structure Exact Match Verification ===${colors.reset}`);
  console.log('Verifying that mock HTML exactly matches backend HTML...\n');
  
  const browser = await chromium.launch();
  const context = await browser.newContext();
  const page = await context.newPage();
  
  const results = [];
  
  // Test all features in English
  console.log(`${colors.blue}=== English Language Tests ===${colors.reset}`);
  
  results.push(await verifyHtmlStructure(page, 'monster-diagnostic', {
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
  
  results.push(await verifyHtmlStructure(page, 'story-generator', {
    endpoint: '/api/story-generator',
    lang: 'en',
    requestBody: {
      name: 'wizard',
      theme: 'adventure',
      style: 'classic',
      language: 'en',
    },
  }));
  
  results.push(await verifyHtmlStructure(page, 'spell-generator', {
    endpoint: '/api/spell-generator',
    lang: 'en',
    requestBody: {
      language: 'en',
    },
  }));
  
  results.push(await verifyHtmlStructure(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'dracula',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'dracula',
      language: 'en',
    },
  }));
  
  results.push(await verifyHtmlStructure(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'witch',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'witch',
      language: 'en',
    },
  }));
  
  results.push(await verifyHtmlStructure(page, 'character-chat', {
    endpoint: '/api/character-chat',
    lang: 'en',
    character: 'jack',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Hello' }]),
      character: 'jack',
      language: 'en',
    },
  }));
  
  results.push(await verifyHtmlStructure(page, 'trivia-bot', {
    endpoint: '/api/trivia-bot',
    lang: 'en',
    requestBody: {
      messages: JSON.stringify([{ role: 'user', content: 'Tell me about Halloween' }]),
      language: 'en',
    },
  }));
  
  // Test Japanese language support
  console.log(`\n${colors.blue}=== Japanese Language Tests ===${colors.reset}`);
  
  results.push(await verifyHtmlStructure(page, 'monster-diagnostic', {
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
  
  results.push(await verifyHtmlStructure(page, 'story-generator', {
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
  
  console.log(`${colors.green}✓ Exact matches: ${successful}${colors.reset}`);
  console.log(`${colors.red}✗ Mismatches: ${failed}${colors.reset}`);
  
  if (failed > 0) {
    console.log(`\n${colors.yellow}Features with mismatches:${colors.reset}`);
    results.filter(r => !r.success).forEach(r => {
      console.log(`  - ${r.feature} (${r.lang || 'en'}): ${r.error || 'HTML structure differs'}`);
    });
  }
  
  // Exit with appropriate code
  process.exit(failed > 0 ? 1 : 0);
}

// Run verification
verifyAll().catch(error => {
  console.error(`${colors.red}Fatal error: ${error.message}${colors.reset}`);
  console.error(error.stack);
  process.exit(1);
});
