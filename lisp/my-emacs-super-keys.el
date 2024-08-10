;;; -*- lexical-binding: t; -*-
;;; 자주 사용 → Super key 할당 → minor mode
;;;  I was inspired by the https://protesilaos.com
(defvar-keymap my-emacs-super-keys-map
  :doc "Common command alternatives using the Super key."
    "s-b" #'consult-buffer
    "s-d" #'consult-dir
    "s-e" #'eshell
    "s-g" #'consult-grep
    "s-o" #'org-insert-structure-template
    "s-r" #'toggle-streaming
    "s-z" #'repeat
    "s-/" #'undo
    "C-s-/" #'undo-redo
    "s-t c"  #'select-special-character
    "s-t k" #'keycast-mode-line-mode
    "s-t m" #'modus-themes-toggle
    "s-t n" #'find-file
    "s-t r" #'toggle-my-reading-mode
    "s-t s" #'my/search-selected-text
    "s-t w" #'my/naver-weather-search
    "s-<return>" #'toggle-frame-fullscreen
    )

  (define-minor-mode my-emacs-super-keys-mode  ; minor-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap my-emacs-super-keys-map)

(provide 'my-emacs-super-keys)
