/**
 * Streaming Handler
 * Handles SSE streaming for form submissions
 */

// Wait for DOM to be ready
document.addEventListener('DOMContentLoaded', () => {
  // Handle HTMX before request event to set up SSE streaming
  document.body.addEventListener('htmx:beforeRequest', (event) => {
  const form = event.detail.elt;
  
  // Check if this is a streaming form (has hx-get attribute pointing to -stream endpoint)
  const url = form.getAttribute('hx-get');
  if (url && url.includes('-stream')) {
    // Get the target container
    const targetId = form.getAttribute('hx-target');
    const target = document.querySelector(targetId);
    
    if (target) {
      // Show loading indicator by adding htmx-request class to form
      form.classList.add('htmx-request');
      
      const indicatorId = form.getAttribute('hx-indicator');
      const indicator = indicatorId ? document.querySelector(indicatorId) : null;
      // Build SSE URL with form data
      const formData = new FormData(form);
      const params = new URLSearchParams();
      
      // Manually add each form field to ensure proper encoding
      for (const [key, value] of formData.entries()) {
        params.append(key, value);
      }
      
      const sseUrl = `${url}?${params.toString()}`;
      console.log('SSE URL:', sseUrl);
      console.log('Form data:', Array.from(formData.entries()));
      
      // Create EventSource for SSE
      const eventSource = new EventSource(sseUrl);
      
      // Store event source on target for cleanup
      target._eventSource = eventSource;
      
      // Variable to track the current streaming bubble
      let currentBubble = null;
      let firstMessageReceived = false;
      
      // Handle start of assistant response
      eventSource.addEventListener('start-response', (e) => {
        const data = JSON.parse(e.data);
        
        // Check if this is a chat response (has character field)
        if (data.character) {
          // Chat responses (Character Chat, Trivia Bot) - preserve history
          const characterName = data.character;
          
          // Create the chat bubble structure
          const chatDiv = document.createElement('div');
          chatDiv.className = 'chat chat-start';
          
          const headerDiv = document.createElement('div');
          headerDiv.className = 'chat-header';
          headerDiv.textContent = characterName;
          
          const bubbleDiv = document.createElement('div');
          bubbleDiv.className = 'chat-bubble chat-bubble-secondary';
          
          chatDiv.appendChild(headerDiv);
          chatDiv.appendChild(bubbleDiv);
          target.appendChild(chatDiv);
          
          // Store reference to the bubble for adding chunks
          currentBubble = bubbleDiv;
        } 
        // Check if this is a spell generator response (has feature field)
        else if (data.feature === 'spell-generator') {
          // Spell generator - clear previous results
          target.innerHTML = '';
          
          // Create the spell container structure with prominent styling
          const containerDiv = document.createElement('div');
          containerDiv.className = 'text-center';
          
          const contentDiv = document.createElement('div');
          contentDiv.className = 'whitespace-pre-wrap';
          // Use larger, more prominent styling similar to spell-phrase
          contentDiv.style.fontSize = '1.5rem';
          contentDiv.style.fontWeight = '600';
          contentDiv.style.lineHeight = '1.6';
          contentDiv.style.maxWidth = '800px';
          contentDiv.style.margin = '1.5rem auto';
          contentDiv.style.fontFamily = 'Georgia, serif';
          contentDiv.style.letterSpacing = '0.01em';
          
          containerDiv.appendChild(contentDiv);
          target.appendChild(containerDiv);
          
          // Store reference to the content div for adding chunks
          currentBubble = contentDiv;
        }
        // Check if this is a monster diagnostic response (has feature field)
        else if (data.feature === 'monster-diagnostic') {
          // Monster diagnostic - clear previous results
          target.innerHTML = '';
          
          const title = data.title || 'Diagnosis Result';
          
          // Create the alert container structure
          const alertDiv = document.createElement('div');
          alertDiv.className = 'alert alert-success';
          
          const titleH3 = document.createElement('h3');
          titleH3.className = 'font-bold text-lg mb-2';
          titleH3.textContent = title;
          
          const contentDiv = document.createElement('div');
          contentDiv.className = 'whitespace-pre-wrap';
          
          alertDiv.appendChild(titleH3);
          alertDiv.appendChild(contentDiv);
          target.appendChild(alertDiv);
          
          // Store reference to the content div for adding chunks
          currentBubble = contentDiv;
        }
      });
      
      // Handle incoming message chunks
      eventSource.addEventListener('message', (e) => {
        // Hide loading indicator on first message
        if (!firstMessageReceived) {
          form.classList.remove('htmx-request');
          firstMessageReceived = true;
        }
        
        if (currentBubble) {
          // Append chunk to the current bubble
          currentBubble.insertAdjacentHTML('beforeend', e.data);
        } else {
          // No current bubble, append as regular HTML (for user messages)
          target.insertAdjacentHTML('beforeend', e.data);
        }
      });
      
      // Handle completion
      eventSource.addEventListener('done', () => {
        currentBubble = null;
        eventSource.close();
        delete target._eventSource;
        
        // Hide loading indicator by removing htmx-request class
        form.classList.remove('htmx-request');
      });
      
      // Handle errors
      eventSource.addEventListener('error', (e) => {
        console.error('SSE Error:', e);
        eventSource.close();
        delete target._eventSource;
        
        // Hide loading indicator by removing htmx-request class
        form.classList.remove('htmx-request');
        
        // Display error message
        target.innerHTML = '<div class="alert alert-error"><span>接続エラーが発生しました</span></div>';
      });
      
      // Cancel the HTMX request since we're handling it with EventSource
      event.preventDefault();
    }
  }
  });

  // Clean up EventSource on page unload
  window.addEventListener('beforeunload', () => {
    document.querySelectorAll('[hx-ext*="sse"]').forEach(el => {
      if (el._eventSource) {
        el._eventSource.close();
        delete el._eventSource;
      }
    });
  });
});
