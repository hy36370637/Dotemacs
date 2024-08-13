;;; -*- lexical-binding: t; -*-
;;; Hyper key  → minor mode
;;;  I was inspired by the https://protesilaos.com
(defvar-keymap my-emacs-hyper-keys-map
  :doc "My command alternatives using the Hyper key."
    ;;; window
    "H-w ]" #'enlarge-window-horizontally
    "H-w ["  #'shrink-window-horizontally
    "H-w ="  #'balance-windows
    "H-w _"  #'shrink-window
    "H-w +"  #'enlarge-window
    
    ;;; kmacro
    "H-k ["  #'kmacro-start-macro
    "H-k ]"  #'kmacro-end-macro
    "H-'"   #'kmacro-end-and-call-macro  ;F4
    )

  (define-minor-mode my-emacs-hyper-keys-mode           ; minor-mode
    "Provide Hyper key alternatives to Command."
    :global t
    :init-value t
    :keymap my-emacs-hyper-keys-map)



;; end here
(provide 'my-emacs-hyper-keys)
