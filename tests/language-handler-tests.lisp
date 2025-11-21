(in-package :kabotan.tests)

;;; Language Handler Tests

(def-suite language-handler-suite
  :in kabotan-suite
  :description "Language handler tests")

(in-suite language-handler-suite)

(test get-ui-text-japanese
  "Test that get-ui-text retrieves Japanese text correctly"
  (is (string= "ハロウィン体験" (get-ui-text :app-title "ja")))
  (is (string= "言語" (get-ui-text :language-selector "ja")))
  (is (string= "モンスター診断" (get-ui-text :feature-monster-diagnostic "ja"))))

(test get-ui-text-english
  "Test that get-ui-text retrieves English text correctly"
  (is (string= "Halloween Experience" (get-ui-text :app-title "en")))
  (is (string= "Language" (get-ui-text :language-selector "en")))
  (is (string= "Monster Diagnostic" (get-ui-text :feature-monster-diagnostic "en"))))

(test get-ui-text-default-to-english
  "Test that get-ui-text defaults to English for unknown language"
  (is (string= "Halloween Experience" (get-ui-text :app-title "fr")))
  (is (string= "Language" (get-ui-text :language-selector "de"))))

(test get-ui-text-missing-key
  "Test that get-ui-text returns nil for missing key"
  (is (null (get-ui-text :nonexistent-key "ja")))
  (is (null (get-ui-text :nonexistent-key "en"))))

(test detect-browser-language-japanese
  "Test that detect-browser-language detects Japanese"
  (is (string= "ja" (detect-browser-language "ja,en-US;q=0.9,en;q=0.8")))
  (is (string= "ja" (detect-browser-language "ja-JP,ja;q=0.9,en;q=0.8")))
  (is (string= "ja" (detect-browser-language "ja"))))

(test detect-browser-language-english
  "Test that detect-browser-language defaults to English"
  (is (string= "en" (detect-browser-language "en-US,en;q=0.9")))
  (is (string= "en" (detect-browser-language "en")))
  (is (string= "en" (detect-browser-language "fr,de;q=0.9"))))

(test detect-browser-language-nil-or-invalid
  "Test that detect-browser-language handles nil or invalid input"
  (is (string= "en" (detect-browser-language nil)))
  (is (string= "en" (detect-browser-language "")))
  (is (string= "en" (detect-browser-language 123))))
