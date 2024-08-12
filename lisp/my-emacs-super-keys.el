;;; -*- lexical-binding: t; -*-
;;; Super key  → minor mode
;;;  I was inspired by the https://protesilaos.com
(defvar-keymap my-emacs-super-keys-map
  :doc "Common command alternatives using the Super key."
    "s-b" #'consult-buffer
    "s-d" #'consult-dir
    "s-e" #'eval-buffer
    "s-g" #'consult-grep
    "s-r" #'toggle-streaming
    "s-z" #'repeat
    "s-/" #'undo
    "C-s-/" #'undo-redo
;; org-mode    
    "s-o e" #'org-emphasize
    "s-o f" #'org-footnote-action
    "s-o o" #'org-insert-structure-template
    "s-o s" #'org-save-all-org-buffers
    "s-t c"  #'select-special-character
    "s-t k" #'keycast-mode-line-mode
    "s-t m" #'modus-themes-toggle
    "s-t r" #'toggle-my-reading-mode
    "s-t s" #'my/search-selected-text
    "s-<return>" #'toggle-frame-fullscreen
    )

  (define-minor-mode my-emacs-super-keys-mode  ; minor-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap my-emacs-super-keys-map)


;; end here
(provide 'my-emacs-super-keys)
