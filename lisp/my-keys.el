;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; Sub-Prefix Maps
(defvar-keymap my-search-prefix-map
  :name "Search"
  "c" #'my-consult-ripgrep-selected-dir
  "f" #'consult-find
  "g" #'consult-grep
  "l" #'consult-line
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
  "b" #'eval-buffer
  "c" #'my-capture-cReading-access
  "i" my-image-prefix-map
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "w" #'my-pair-pairs-wrap
  ;; Radio: Just Play and Stop
  "p" #'my-radio-play 
  "k" #'my-radio-stop) 

;; which-key Labels
(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "c" "Reading"
  "i" "Images"
  "s" "Search"
  "p" "Radio Play"
  "k" "Radio Stop"
  "t" "Today's"
  "w" "Pairs Wrap")

(keymap-set global-map "C-c j" my-emacs-prefix-map)

(provide 'my-keys)
