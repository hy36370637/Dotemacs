;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; =======================================
;;; Sub-Prefix Maps
;; =======================================
(defvar-keymap my-edit-prefix-map
  :name "Edit"
  "a" #'my-newline-above
  "c" #'my-select-current-line
  "d" #'my-duplicate-dwim
  "l" #'my-select-line-left
  "r" #'my-select-line-right
  "R" #'my-query-replace-regexp-dwim
  "n" #'my-newline
  "t" #'my-today-stamp
  "w" #'my-pair-pairs-wrap)

(defvar-keymap my-capture-prefix-map
  :name "Capture"
  "c" #'org-capture
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

(defvar-keymap my-media-prefix-map
  :name "Media"
  "P" #'my-radio-play 
  "S" #'my-radio-stop
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
  "e" my-edit-prefix-map
  "m" my-media-prefix-map
  "r" #'jump-to-register
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "v" #'view-mode)

;; =======================================
;;; Which-key lable
;; =======================================
(which-key-add-keymap-based-replacements my-edit-prefix-map
  "a" "Above line"
  "c" "Current Line"
  "d" "Duplicate"
  "l" "Left select"
  "r" "Right select"
  "R" "Regexp replace"
  "n" "New line"
  "t" "Today"
  "w" "Wrap")

(which-key-add-keymap-based-replacements my-capture-prefix-map
  "c" "Capture Menu"
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
  "r" "Range"
  "w" "Weather")

(which-key-add-keymap-based-replacements my-media-prefix-map
  "P" "Play radio"
  "S" "Stop radio"
  "i" "Insert img"
  "p" "Path img"
  "s" "Screenshot")

(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "b" "Bookmark"
  "c" "Capture"
  "e" "Edit"
  "m" "Media"
  "r" "Register"
  "s" "Search"
  "t" "Today's"
  "v" "View-mode")
;; =======================================
;;; Key-binding
;; =======================================
 (keymap-set global-map "M-SPC" my-emacs-prefix-map)


(provide 'my-keys)
;;; my-keys end here
