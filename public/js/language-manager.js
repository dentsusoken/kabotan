// Language Manager Module
// Simplified for HTMX-driven architecture
// Handles only localStorage language preference management

// Current language state
let currentLang = 'ja';

// Translation dictionary for UI elements
const translations = {
    ja: {
        'tab-monster': '👹 モンスター診断',
        'tab-story': '📖 ストーリー生成',
        'tab-chat': '💬 キャラクターチャット',
        'tab-trivia': '🎓 トリビアボット',
        'tab-spell': '✨ 呪文生成'
    },
    en: {
        'tab-monster': '👹 Monster Diagnostic',
        'tab-story': '📖 Story Generator',
        'tab-chat': '💬 Character Chat',
        'tab-trivia': '🎓 Trivia Bot',
        'tab-spell': '✨ Spell Generator'
    }
};

/**
 * Update UI text based on current language
 */
function updateUIText() {
    const elements = document.querySelectorAll('[data-i18n]');
    elements.forEach(element => {
        const key = element.getAttribute('data-i18n');
        if (translations[currentLang] && translations[currentLang][key]) {
            element.textContent = translations[currentLang][key];
        }
    });
}

/**
 * Initialize language from localStorage or browser detection
 */
function initLanguage() {
    const savedLang = localStorage.getItem('language');
    if (savedLang && (savedLang === 'ja' || savedLang === 'en')) {
        // Use saved language preference if valid
        currentLang = savedLang;
    } else {
        // Detect browser language on first load or if saved language is invalid
        const browserLang = navigator.language || navigator.userLanguage;
        // Set to 'ja' if browser language starts with 'ja', otherwise 'en'
        currentLang = browserLang.startsWith('ja') ? 'ja' : 'en';
        // Save the detected language to localStorage
        localStorage.setItem('language', currentLang);
    }
    
    // Apply language to document
    document.documentElement.setAttribute('data-lang', currentLang);
    document.documentElement.setAttribute('lang', currentLang);
    
    // Update language toggle to match current language
    const toggle = document.getElementById('language-toggle');
    if (toggle) {
        toggle.checked = currentLang === 'en';
    }
    
    // Update UI text
    updateUIText();
    
    // Update HTMX requests to include current language
    updateHtmxLanguage();
}

/**
 * Update HTMX requests to include current language parameter
 */
function updateHtmxLanguage() {
    // Update all HTMX elements to include language parameter
    document.querySelectorAll('[hx-get], [hx-post]').forEach(element => {
        const currentVals = element.getAttribute('hx-vals');
        let valsObj = {};
        
        // Parse existing hx-vals if present
        if (currentVals) {
            try {
                valsObj = JSON.parse(currentVals);
            } catch (e) {
                console.warn('Failed to parse hx-vals:', currentVals);
            }
        }
        
        // Update language in vals object
        valsObj.language = currentLang;
        
        // Set updated hx-vals
        element.setAttribute('hx-vals', JSON.stringify(valsObj));
    });
}

/**
 * Setup HTMX event listeners to update language on dynamic content
 */
function setupHtmxEventListeners() {
    // Update language parameter after content is swapped
    document.body.addEventListener('htmx:afterSwap', function(evt) {
        updateHtmxLanguage();
    });
    
    // Update language parameter after content is settled
    document.body.addEventListener('htmx:afterSettle', function(evt) {
        updateHtmxLanguage();
    });
}

/**
 * Get current language
 * @returns {string} - Current language code ('ja' or 'en')
 */
function getCurrentLanguage() {
    return currentLang;
}

/**
 * Setup language toggle event listener
 */
function setupLanguageToggle() {
    const toggle = document.getElementById('language-toggle');
    if (toggle) {
        toggle.addEventListener('change', function(e) {
            currentLang = e.target.checked ? 'en' : 'ja';
            localStorage.setItem('language', currentLang);
            document.documentElement.setAttribute('data-lang', currentLang);
            document.documentElement.setAttribute('lang', currentLang);
            
            // Update UI text
            updateUIText();
            
            // Update HTMX requests with new language
            updateHtmxLanguage();
            
            // Reload current feature content with new language using set-language endpoint
            const activeTab = document.querySelector('.tab.tab-active');
            if (activeTab) {
                const feature = activeTab.getAttribute('data-feature');
                if (feature) {
                    // Use POST /api/set-language endpoint to update session and reload content
                    htmx.ajax('POST', '/api/set-language', {
                        target: '#feature-content',
                        swap: 'innerHTML',
                        values: {
                            language: currentLang,
                            feature: feature
                        }
                    });
                }
            }
        });
    }
}

// Initialize on DOM ready
document.addEventListener('DOMContentLoaded', function() {
    initLanguage();
    setupLanguageToggle();
    setupHtmxEventListeners();
});
