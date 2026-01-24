;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; ======================================
;;; Helper Functions
;; ======================================
;; (defun my/invoke-my-prefix ()
;;   (interactive)
;;   (my/prefix-with-english my-emacs-prefix-map))

;; =======================================
;;; Sub-Prefix Maps
;; =======================================
(defvar-keymap my-edit-prefix-map
  :name "Edit"
  ;; "d" #'my-pair-delete
  "r" #'my-query-replace-regexp-dwim
  "t" #'my-today-stamp
  "w" #'my-pair-pairs-wrap)

(defvar-keymap my-line-prefix-map
  :name "Line"
  "a" #'my-newline-above
  "c" #'my-select-current-line
  "d" #'my-duplicate-dwim
  "l" #'my-select-line-left
  "r" #'my-select-line-right
  "n" #'my-newline)

(defvar-keymap my-search-prefix-map
  :name "Search"
  "f" #'consult-find
  "g" #'consult-grep
  "l" #'consult-line
  "o" #'consult-outline
  "u" #'my-search-unified
  "m" #'consult-imenu
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
  "e" my-edit-prefix-map
  "l" my-line-prefix-map
  "m" my-media-prefix-map
  "r" #'jump-to-register
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "v" #'view-mode)

;; =======================================
;;; Which-key lable
;; =======================================
(which-key-add-keymap-based-replacements my-edit-prefix-map
  ;; "d" "pairs delete"
  "r" "Regexp replace"
  "t" "Today stamp"
  "w" "pairs Wrap")

(which-key-add-keymap-based-replacements my-line-prefix-map
  "a" "Above line"
  "c" "Current Line"
  "d" "Duplicate"
  "l" "Left select"
  "r" "Right select"
  "n" "New line")

(which-key-add-keymap-based-replacements my-search-prefix-map
  "f" "Find"
  "g" "Grep"
  "l" "Line"
  "o" "Outline"
  "u" "Unified Search"
  "m" "Imenu"
  "w" "Weather")

(which-key-add-keymap-based-replacements my-media-prefix-map
  "P" "Play radio"
  "S" "Stop radio"
  "i" "Insert img"
  "p" "Path img"
  "s" "Screenshot")

(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "e" "Edit"
  "l" "Line"
  "m" "Media"
  "r" "Register"
  "s" "Search"
  "t" "Today's"
  "v" "Viewer")

;; =======================================
;;; Key-binding
;; =======================================
(keymap-set global-map "M-SPC" my-emacs-prefix-map)


(provide 'my-keys)
;;; my-keys.el end here
