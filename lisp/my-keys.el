;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; =======================================
;;; Sub-Prefix Maps
;; =======================================
(defvar-keymap my-edit-prefix-map
  :name "Edit"
  "a" #'my-newline-above
  "l" #'my-select-current-line
  "n" #'my-newline
  "t" #'my-today-stamp
  "w" #'my-pair-pairs-wrap)

(defvar-keymap my-capture-prefix-map
  :name "Capture"
  "c" #'org-capture
  "a" #'my-capture-cReading-access
  "d" (lambda () (interactive) (org-capture nil "d"))
  "t" (lambda () (interactive) (org-capture nil "t"))
  "r" (lambda () (interactive) (org-capture nil "r"))
  "m" (lambda () (interactive) (org-capture nil "m")))

(defvar-keymap my-search-prefix-map
  :name "Search"
  "c" #'my-consult-ripgrep-selected-dir
  "f" #'consult-find
  "g" #'consult-grep
  "l" #'consult-line
  "o" #'consult-outline
  "m" #'consult-imenu
  "r" #'my-search-in-range
  "w" #'my-search-weather)

(defvar-keymap my-image-prefix-map
  :name "Image"
  "i" #'my-org-insert-image
  "p" #'my-insert-image-path
  "s" #'my-org-screenshot)

;; =======================================
;;; Master Keymap
;; =======================================
(defvar-keymap my-emacs-prefix-map
  :name "Master"
  "b" #'consult-bookmark
  "c" my-capture-prefix-map
  "C" #'my-capture-cReading-access
  "e" my-edit-prefix-map
  "i" my-image-prefix-map
  "r" #'jump-to-register
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "p" #'my-radio-play 
  "k" #'my-radio-stop)

;; =======================================
;;; Which-key lable
;; =======================================
(which-key-add-keymap-based-replacements my-edit-prefix-map
  "a" "lineAbove"
  "l" "selLine"
  "n" "Newline"
  "t" "Today"
  "w" "pairWrap")

(which-key-add-keymap-based-replacements my-capture-prefix-map
  "c" "Capture Menu"
  "a" "cReadNow"
  "d" "Daily"
  "t" "Tasks"
  "r" "Reading"
  "m" "경조사")

(which-key-add-keymap-based-replacements my-search-prefix-map
  "c" "ripgrep"
  "f" "Find"
  "g" "Grep"
  "l" "Line"
  "o" "Outline"
  "m" "Imenu"
  "r" "sRange"
  "w" "Weather")

(which-key-add-keymap-based-replacements my-image-prefix-map
  "i" "Insert"
  "p" "Path"
  "s" "Screenshot")

(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "b" "Bookmark"
  "c" "Capture"
  "C" "Reading"
  "e" "Edit"
  "i" "Images"
  "r" "Register"
  "s" "Search"
  "p" "Radio Play"
  "k" "Radio Stop"
  "t" "Today's")

;; =======================================
;;; Key-binding
;; =======================================
(keymap-set global-map "M-SPC" my-emacs-prefix-map)



(provide 'my-keys)
;;; my-keys end here
