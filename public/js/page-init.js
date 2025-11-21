/**
 * Page Initialization
 * Handles initial page load and default feature display
 */

document.addEventListener('DOMContentLoaded', () => {
  // Get language preference from localStorage or default to Japanese
  const language = localStorage.getItem('language') || 'ja';
  
  // Load default feature (monster-diagnostic) on page load
  const defaultFeature = 'monster-diagnostic';
  const featureContent = document.getElementById('feature-content');
  const tabs = document.querySelectorAll('[role="tab"]');
  
  if (featureContent && tabs.length > 0) {
    // Set initial active tab
    tabs.forEach(tab => {
      const tabFeature = tab.getAttribute('data-feature');
      if (tabFeature === defaultFeature) {
        tab.classList.add('tab-active');
      }
    });
    
    // Use HTMX to load the default feature
    htmx.ajax('GET', `/api/features/${defaultFeature}?language=${language}`, {
      target: '#feature-content',
      swap: 'innerHTML'
    });
    
    // Set up tab click handlers for active state management
    tabs.forEach(tab => {
      tab.addEventListener('click', function() {
        // Remove active class from all tabs
        tabs.forEach(t => t.classList.remove('tab-active'));
        // Add active class to clicked tab
        this.classList.add('tab-active');
      });
    });
    
    // Listen for HTMX afterSwap event to ensure tab state is correct
    document.body.addEventListener('htmx:afterSwap', (event) => {
      // If the swap was for feature content, update tab state is correct
      if (event.detail.target && event.detail.target.id === 'feature-content') {
        const requestUrl = event.detail.xhr && event.detail.xhr.responseURL;
        if (requestUrl) {
          const featureName = requestUrl.match(/\/features\/([^?]+)/)?.[1];
          
          if (featureName) {
            tabs.forEach(tab => {
              const tabFeature = tab.getAttribute('data-feature');
              if (tabFeature === featureName) {
                tab.classList.add('tab-active');
              } else {
                tab.classList.remove('tab-active');
              }
            });
          }
        }
      }
    });
  }
});
