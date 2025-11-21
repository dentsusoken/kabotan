(in-package :kabotan.tests)

;;; Response Formatting Tests

(def-suite response-formatting-suite
  :in kabotan-suite
  :description "Response formatting utility tests")

(in-suite response-formatting-suite)

(test format-html-response-basic
  "Test that format-html-response produces valid DaisyUI HTML"
  (let ((result (format-html-response "test content")))
    (is (search "card bg-base-200 shadow-xl" result))
    (is (search "card-body" result))
    (is (search "test content" result))))

(test format-html-response-custom-class
  "Test that format-html-response accepts custom class"
  (let ((result (format-html-response "content" :class "custom-class")))
    (is (search "custom-class" result))
    (is (search "card-body" result))))

;;; Note: format-monster-diagnostic-response and format-story-response tests removed
;;; as those functions have been removed from the codebase

(test format-spell-generator-response-valid
  "Test that format-spell-generator-response produces valid DaisyUI HTML"
  (let ((result (format-spell-generator-response "Spell: Abracadabra! Meaning: A powerful spell." "en")))
    (is (search "text-center" result))
    (is (search "spell-phrase" result))
    (is (search "spell-explanation" result))
    (is (search "Abracadabra!" result))
    (is (search "A powerful spell." result))))
