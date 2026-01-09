;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; Sub-Prefix Maps
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

;; Master Keymap (Flattened & Simplified)
(defvar-keymap my-emacs-prefix-map
  :name "Master"
  "c" #'my-capture-cReading-access
  "i" my-image-prefix-map
  "j" #'jump-to-register
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "w" #'my-pair-pairs-wrap
  ;; Radio: Just Play and Stop
  "p" #'my-radio-play 
  "k" #'my-radio-stop) 

;; which-key Labels
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
  "c" "Reading"
  "i" "Images"
  "j" "Jump Register"
  "s" "Search"
  "p" "Radio Play"
  "k" "Radio Stop"
  "t" "Today's"
  "w" "Pairs Wrap")

(keymap-set global-map "C-c j" my-emacs-prefix-map)

(provide 'my-keys)
