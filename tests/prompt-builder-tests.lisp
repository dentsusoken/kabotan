(in-package :kabotan.tests)

;;; Prompt Builder Tests

(def-suite prompt-builder-suite
  :in kabotan-suite
  :description "Prompt builder tests")

(in-suite prompt-builder-suite)

;;; Note: Legacy prompt builder function tests have been removed as those functions
;;; are no longer used. Current implementation uses system prompts with the messages API.

(test placeholder-test
  "Placeholder test to keep suite valid"
  (is-true t "Prompt builder tests removed - using system prompts with messages API now"))
