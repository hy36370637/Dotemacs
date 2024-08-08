;;; -*- lexical-binding: t; -*-
;;; 자주 사용 → Super key 할당 → minor mode
;;;  I was inspired by the https://protesilaos.com
(defvar-keymap my-emacs-super-keys-map
  :doc "Common command alternatives using the Super key."
    "s-b" #'consult-buffer
    "s-d" #'consult-dir
    "s-e" #'eshell
    "s-g" #'consult-grep
    "s-n" #'find-file
    "s-o" #'org-insert-structure-template
    "s-r" #'toggle-streaming
    "s-z" #'repeat
    "s-<return>" #'toggle-frame-fullscreen
    "s-/" #'undo-redo
    "C-s-k" #'keycast-mode-line-mode
    "C-s-r" #'toggle-my-reading-mode
    "C-s-s" #'my/region-search-web
    "C-s-w" #'my/naver-weather-search
    "s-t c"  #'select-special-character
    "s-t m" #'modus-themes-toggle
    "s-t r" #'toggle-my-reading-mode
    "s-t s" #'my/search-selected-text
    "s-t w" #'my/naver-weather-search
    )

  (define-minor-mode my-emacs-super-keys-mode  ; minor-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap my-emacs-super-keys-map)

(provide 'my-emacs-super-keys)
