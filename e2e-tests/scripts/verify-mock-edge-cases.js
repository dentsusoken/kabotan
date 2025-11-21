/**
 * Mock Response Edge Cases Verification Script
 * 
 * This script tests edge cases for mock responses to ensure they handle
 * various scenarios correctly.
 * 
 * Usage:
 *   node e2e-tests/scripts/verify-mock-edge-cases.js
 */

const { test, expect } = require('@playwright/test');
const { 
  createMockNonStreamingResponse, 
  createStreamingChunks,
  MOCK_RESPONSES 
} = require('../helpers/mock-llm-api');

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
 * Test result tracker
 */
const results = [];

function logTest(name, passed, details = '') {
  results.push({ name, passed, details });
  const icon = passed ? '✓' : '✗';
  const color = passed ? colors.green : colors.red;
  console.log(`${color}${icon} ${name}${colors.reset}`);
  if (details && !passed) {
    console.log(`  ${colors.yellow}${details}${colors.reset}`);
  }
}

/**
 * Test empty responses
 */
function testEmptyResponses() {
  console.log(`\n${colors.cyan}Testing Empty Responses...${colors.reset}`);
  
  // Empty custom response should still generate valid HTML
  const emptyResponse = createMockNonStreamingResponse('character-chat', 'en', {
    character: 'dracula',
    customResponse: '',
  });
  
  const hasValidStructure = emptyResponse.includes('<div class="chat chat-start">');
  logTest('Empty response generates valid HTML structure', hasValidStructure);
}

/**
 * Test very long responses
 */
function testLongResponses() {
  console.log(`\n${colors.cyan}Testing Long Responses...${colors.reset}`);
  
  // Generate a very long response (10,000 characters)
  const longText = 'A'.repeat(10000);
  const longResponse = createMockNonStreamingResponse('story-generator', 'en', {
    customResponse: longText,
  });
  
  const hasValidStructure = longResponse.includes('<div class="bg-base-300 rounded-lg p-4">');
  const containsText = longResponse.includes('A'.repeat(100)); // Check partial content
  
  logTest('Long response generates valid HTML structure', hasValidStructure);
  logTest('Long response contains expected content', containsText);
}

/**
 * Test special characters
 */
function testSpecialCharacters() {
  console.log(`\n${colors.cyan}Testing Special Characters...${colors.reset}`);
  
  const specialChars = '<script>alert("XSS")</script> & "quotes" \'apostrophes\'';
  const response = createMockNonStreamingResponse('trivia-bot', 'en', {
    customResponse: specialChars,
  });
  
  // Check that special characters are escaped
  const hasEscapedLt = response.includes('&lt;');
  const hasEscapedGt = response.includes('&gt;');
  const hasEscapedAmp = response.includes('&amp;');
  const hasEscapedQuot = response.includes('&quot;');
  const noRawScript = !response.includes('<script>');
  
  logTest('Special characters: < escaped', hasEscapedLt);
  logTest('Special characters: > escaped', hasEscapedGt);
  logTest('Special characters: & escaped', hasEscapedAmp);
  logTest('Special characters: " escaped', hasEscapedQuot);
  logTest('Special characters: No raw script tags', noRawScript);
}

/**
 * Test multi-line responses
 */
function testMultiLineResponses() {
  console.log(`\n${colors.cyan}Testing Multi-line Responses...${colors.reset}`);
  
  const multiLineText = 'Line 1\nLine 2\nLine 3\n\nLine 5 (after blank line)';
  const response = createMockNonStreamingResponse('monster-diagnostic', 'en', {
    customResponse: multiLineText,
  });
  
  const hasValidStructure = response.includes('<div class="alert alert-success">');
  const hasWhitespacePreWrap = response.includes('whitespace-pre-wrap');
  
  logTest('Multi-line response generates valid HTML structure', hasValidStructure);
  logTest('Multi-line response preserves whitespace', hasWhitespacePreWrap);
}

/**
 * Test character-specific responses
 */
function testCharacterSpecificResponses() {
  console.log(`\n${colors.cyan}Testing Character-specific Responses...${colors.reset}`);
  
  const characters = [
    { name: 'dracula', emoji: '🧛' },
    { name: 'witch', emoji: '🧙' },
    { name: 'jack', emoji: '🎃' },
  ];
  
  characters.forEach(({ name, emoji }) => {
    const response = createMockNonStreamingResponse('character-chat', 'en', {
      character: name,
    });
    
    const hasCorrectEmoji = response.includes(emoji);
    logTest(`Character ${name} has correct emoji (${emoji})`, hasCorrectEmoji);
  });
}

/**
 * Test language-specific responses
 */
function testLanguageSpecificResponses() {
  console.log(`\n${colors.cyan}Testing Language-specific Responses...${colors.reset}`);
  
  // Test English
  const enResponse = createMockNonStreamingResponse('monster-diagnostic', 'en', {
    customResponse: 'Test content',
  });
  const hasEnTitle = enResponse.includes('Diagnosis Result');
  logTest('English language: Correct title', hasEnTitle);
  
  // Test Japanese
  const jaResponse = createMockNonStreamingResponse('monster-diagnostic', 'ja', {
    customResponse: 'テスト内容',
  });
  const hasJaTitle = jaResponse.includes('診断結果');
  logTest('Japanese language: Correct title', hasJaTitle);
}

/**
 * Test streaming chunks
 */
function testStreamingChunks() {
  console.log(`\n${colors.cyan}Testing Streaming Chunks...${colors.reset}`);
  
  const text = 'Hello, world!';
  const chunks = createStreamingChunks(text, 5);
  
  // Check chunk count (text length / chunk size + 1 for [DONE])
  const expectedChunkCount = Math.ceil(text.length / 5) + 1;
  const hasCorrectChunkCount = chunks.length === expectedChunkCount;
  logTest(`Streaming: Correct chunk count (${chunks.length} === ${expectedChunkCount})`, hasCorrectChunkCount);
  
  // Check SSE format
  const allHaveDataPrefix = chunks.every(chunk => chunk.startsWith('data: '));
  logTest('Streaming: All chunks have "data: " prefix', allHaveDataPrefix);
  
  // Check that all chunks end with double newline (SSE format requirement)
  const allEndWithDoubleNewline = chunks.every(chunk => chunk.endsWith('\n\n'));
  logTest('Streaming: All chunks end with \\n\\n (SSE format)', allEndWithDoubleNewline);
  
  // Check completion marker
  const hasDoneMarker = chunks[chunks.length - 1].includes('[DONE]');
  logTest('Streaming: Has [DONE] marker', hasDoneMarker);
  
  // Check [DONE] marker format (should be exactly "data: [DONE]\n\n")
  const doneMarkerFormat = chunks[chunks.length - 1] === 'data: [DONE]\n\n';
  logTest('Streaming: [DONE] marker has correct format', doneMarkerFormat);
  
  // Check JSON format (except [DONE])
  const contentChunks = chunks.slice(0, -1);
  const allValidJson = contentChunks.every(chunk => {
    try {
      const jsonStr = chunk.replace('data: ', '').trim();
      const parsed = JSON.parse(jsonStr);
      return parsed.choices && parsed.choices[0] && parsed.choices[0].delta;
    } catch {
      return false;
    }
  });
  logTest('Streaming: All content chunks are valid JSON', allValidJson);
  
  // Check that JSON structure matches OpenAI SSE format
  const hasCorrectStructure = contentChunks.every(chunk => {
    try {
      const jsonStr = chunk.replace('data: ', '').trim();
      const parsed = JSON.parse(jsonStr);
      return (
        parsed.choices &&
        Array.isArray(parsed.choices) &&
        parsed.choices[0] &&
        parsed.choices[0].delta &&
        typeof parsed.choices[0].delta.content === 'string'
      );
    } catch {
      return false;
    }
  });
  logTest('Streaming: JSON structure matches OpenAI SSE format', hasCorrectStructure);
  
  // Verify that concatenating all content chunks produces the original text
  let reconstructedText = '';
  contentChunks.forEach(chunk => {
    try {
      const jsonStr = chunk.replace('data: ', '').trim();
      const parsed = JSON.parse(jsonStr);
      reconstructedText += parsed.choices[0].delta.content;
    } catch {
      // Ignore
    }
  });
  const textReconstructedCorrectly = reconstructedText === text;
  logTest('Streaming: Reconstructed text matches original', textReconstructedCorrectly);
}

/**
 * Test chunk timing simulation limitations
 */
function testChunkTimingSimulation() {
  console.log(`\n${colors.cyan}Testing Chunk Timing Simulation...${colors.reset}`);
  
  // Note: Playwright's route.fulfill() does not support true streaming
  // All chunks are sent at once, so timing simulation is not possible
  // This test documents the limitation and verifies the format is correct
  
  const text = 'Test streaming response';
  const chunkSize = 5;
  const chunks = createStreamingChunks(text, chunkSize);
  
  // Verify chunks are created with correct size
  const contentChunks = chunks.slice(0, -1); // Exclude [DONE]
  let totalChars = 0;
  contentChunks.forEach(chunk => {
    try {
      const jsonStr = chunk.replace('data: ', '').trim();
      const parsed = JSON.parse(jsonStr);
      const content = parsed.choices[0].delta.content;
      totalChars += content.length;
      
      // Each chunk should be <= chunkSize (last chunk may be smaller)
      const isValidSize = content.length <= chunkSize;
      if (!isValidSize) {
        logTest(`Chunk timing: Chunk size validation (${content.length} <= ${chunkSize})`, false, 
          `Chunk "${content}" exceeds size limit`);
      }
    } catch (e) {
      logTest('Chunk timing: Chunk parsing failed', false, e.message);
    }
  });
  
  // Verify total characters match original text
  const totalMatches = totalChars === text.length;
  logTest(`Chunk timing: Total characters match (${totalChars} === ${text.length})`, totalMatches);
  
  // Document limitation
  console.log(`  ${colors.yellow}Note: Playwright route.fulfill() limitation${colors.reset}`);
  console.log(`  ${colors.yellow}Actual timing simulation (chunkDelay) is not possible${colors.reset}`);
  console.log(`  ${colors.yellow}All chunks are sent at once in the response body${colors.reset}`);
  console.log(`  ${colors.yellow}Format is correct for SSE, but no real-time streaming${colors.reset}`);
  
  // Verify that the limitation is documented in the code
  const fs = require('fs');
  const mockFilePath = 'e2e-tests/helpers/mock-llm-api.js';
  const mockFileContent = fs.readFileSync(mockFilePath, 'utf-8');
  
  const hasLimitationComment = mockFileContent.includes('Note: We can\'t actually stream in route.fulfill');
  logTest('Chunk timing: Limitation documented in code', hasLimitationComment);
  
  // Test with different chunk sizes
  const testSizes = [1, 3, 5, 10, 20];
  testSizes.forEach(size => {
    const testChunks = createStreamingChunks(text, size);
    const expectedCount = Math.ceil(text.length / size) + 1; // +1 for [DONE]
    const hasCorrectCount = testChunks.length === expectedCount;
    logTest(`Chunk timing: Chunk size ${size} produces correct count`, hasCorrectCount);
  });
}

/**
 * Test spell generator parsing
 */
function testSpellGeneratorParsing() {
  console.log(`\n${colors.cyan}Testing Spell Generator Parsing...${colors.reset}`);
  
  // Test standard format
  const standardSpell = 'Spell: Moonlight Shield\nMeaning: Creates a protective barrier';
  const standardResponse = createMockNonStreamingResponse('spell-generator', 'en', {
    customResponse: standardSpell,
  });
  
  const hasSpellPhrase = standardResponse.includes('<div class="spell-phrase">');
  const hasSpellExplanation = standardResponse.includes('<div class="spell-explanation mt-6">');
  const containsSpellText = standardResponse.includes('Moonlight Shield');
  const containsMeaningText = standardResponse.includes('Creates a protective barrier');
  
  logTest('Spell generator: Has spell phrase div', hasSpellPhrase);
  logTest('Spell generator: Has spell explanation div', hasSpellExplanation);
  logTest('Spell generator: Contains spell text', containsSpellText);
  logTest('Spell generator: Contains meaning text', containsMeaningText);
  
  // Test malformed format (missing "Spell:" or "Meaning:")
  const malformedSpell = 'Just some random text without proper format';
  const malformedResponse = createMockNonStreamingResponse('spell-generator', 'en', {
    customResponse: malformedSpell,
  });
  
  const stillHasStructure = malformedResponse.includes('<div class="text-center">');
  logTest('Spell generator: Handles malformed input gracefully', stillHasStructure);
}

/**
 * Main test runner
 */
function runAllTests() {
  console.log(`${colors.blue}=== Mock Response Edge Cases Verification ===${colors.reset}`);
  
  testEmptyResponses();
  testLongResponses();
  testSpecialCharacters();
  testMultiLineResponses();
  testCharacterSpecificResponses();
  testLanguageSpecificResponses();
  testStreamingChunks();
  testChunkTimingSimulation();
  testSpellGeneratorParsing();
  
  // Print summary
  console.log(`\n${colors.blue}=== Test Summary ===${colors.reset}`);
  
  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;
  
  console.log(`${colors.green}✓ Passed: ${passed}${colors.reset}`);
  console.log(`${colors.red}✗ Failed: ${failed}${colors.reset}`);
  
  if (failed > 0) {
    console.log(`\n${colors.yellow}Failed tests:${colors.reset}`);
    results.filter(r => !r.passed).forEach(r => {
      console.log(`  - ${r.name}`);
      if (r.details) {
        console.log(`    ${r.details}`);
      }
    });
  }
  
  // Exit with appropriate code
  process.exit(failed > 0 ? 1 : 0);
}

// Run all tests
runAllTests();
