const { defineConfig, devices } = require('@playwright/test');

module.exports = defineConfig({
  testDir: './e2e-tests',
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: 'html',
  timeout: 360000, // 360 seconds (6 minutes) per test - LLM responses can take up to 5 minutes
  expect: {
    timeout: 60000, // 60 seconds for assertions - allow time for LLM-generated content to appear
  },
  use: {
    baseURL: 'http://localhost:5000',
    trace: 'on-first-retry',
    screenshot: 'only-on-failure',
    actionTimeout: 60000, // 60 seconds for actions - LLM-dependent UI updates may be slow
    navigationTimeout: 30000, // 30 seconds for navigation
  },
  projects: [
    {
      name: 'ui-tests',
      testDir: './e2e-tests/ui',
      timeout: 30000, // 30 seconds per test - UI tests with mocks are fast
      use: { 
        ...devices['Desktop Chrome'],
        baseURL: 'http://localhost:5000',
        // Disable cache to ensure fresh JavaScript files are loaded
        launchOptions: {
          args: ['--disable-http-cache', '--disable-cache', '--disable-application-cache']
        },
      },
    },
    {
      name: 'integration-tests',
      testDir: './e2e-tests/integration',
      timeout: 180000, // 180 seconds per test - integration tests with real LLM are slow
      use: { 
        ...devices['Desktop Chrome'],
        baseURL: 'http://localhost:5000',
        // Disable cache to ensure fresh JavaScript files are loaded
        launchOptions: {
          args: ['--disable-http-cache', '--disable-cache', '--disable-application-cache']
        },
      },
    },
  ],
  webServer: {
    command: 'make run',
    url: 'http://localhost:5000',
    reuseExistingServer: !process.env.CI,
    timeout: 120000,
  },
});
