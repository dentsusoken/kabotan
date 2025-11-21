(in-package :kabotan.tests)

;;; HTML Form Components Tests

(def-suite html-forms-suite
  :in kabotan-suite
  :description "HTML form components tests")

(in-suite html-forms-suite)

;;; Radio Button Group Tests

(test generate-form-radio-group-has-gap-8-in-outer-container
  "Test that generate-form-radio-group includes gap-8 in outer container"
  (let ((result (kabotan::generate-form-radio-group
                 :name "test-radio"
                 :label "Test Radio Group"
                 :options '(("option1" . "Option 1")
                           ("option2" . "Option 2")
                           ("option3" . "Option 3")))))
    (is (search "gap-8" result))
    (is (search "flex flex-wrap gap-8" result))))

(test generate-form-radio-group-has-gap-3-in-label
  "Test that generate-form-radio-group includes gap-3 in label elements"
  (let ((result (kabotan::generate-form-radio-group
                 :name "test-radio"
                 :label "Test Radio Group"
                 :options '(("option1" . "Option 1")
                           ("option2" . "Option 2")))))
    (is (search "gap-3" result))
    (is (search "items-center gap-3" result))))

(test generate-form-radio-group-has-px-3-in-label
  "Test that generate-form-radio-group includes px-3 in label elements"
  (let ((result (kabotan::generate-form-radio-group
                 :name "test-radio"
                 :label "Test Radio Group"
                 :options '(("option1" . "Option 1")
                           ("option2" . "Option 2")))))
    (is (search "px-3" result))
    (is (search "gap-3 px-3" result))))

(test generate-form-radio-group-has-min-w-fit-in-label
  "Test that generate-form-radio-group includes min-w-fit in label elements"
  (let ((result (kabotan::generate-form-radio-group
                 :name "test-radio"
                 :label "Test Radio Group"
                 :options '(("option1" . "Option 1")
                           ("option2" . "Option 2")))))
    (is (search "min-w-fit" result))
    (is (search "px-3 min-w-fit" result))))

(test generate-form-radio-group-all-styling-classes-present
  "Test that generate-form-radio-group includes all required styling classes"
  (let ((result (kabotan::generate-form-radio-group
                 :name "character"
                 :label "Character"
                 :options '(("dracula" . "Dracula")
                           ("witch" . "Witch")
                           ("jack" . "Jack-o'-Lantern"))
                 :selected "dracula")))
    ;; Outer container should have gap-8
    (is (search "flex flex-wrap gap-8" result))
    ;; Labels should have gap-3, px-3, and min-w-fit
    (is (search "cursor-pointer flex items-center gap-3 px-3 min-w-fit" result))
    ;; Should have proper radio button structure
    (is (search "type=\"radio\"" result))
    (is (search "name=\"character\"" result))
    (is (search "value=\"dracula\"" result))
    (is (search "checked" result))
    (is (search "Dracula" result))
    (is (search "Witch" result))
    (is (search "Jack-o&#39;-Lantern" result))))
