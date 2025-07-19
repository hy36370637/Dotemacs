;;; -*- lexical-binding: t; -*-
;;; Hyper and Super keys → minor mode
;;; I was inspired by the https://protesilaos.com

(defvar-keymap my-emacs-super-keys-map
  :doc "My command alternatives using the Super key."
  "s-1"  #'tab-bar-switch-to-prev-tab	;bab-bar move left
  "s-2"  #'tab-bar-switch-to-next-tab	;tab-bar move right
  "s-z"   #'repeat
  "s-/"   #'undo
  "C-s-/" #'undo-redo
  "s-<return>" #'toggle-frame-fullscreen
;;  "M-s-<return>" #'toggle-frame-maximized   ; M-F10
)

(define-minor-mode my-emacs-hyper-super-keys-mode
  "Provide Hyper and Super key alternatives to Command."
  :global t
  :init-value t
  ;; :keymap (make-composed-keymap (list my-emacs-hyper-keys-map
  ;;                                     my-emacs-super-keys-map)))
  :keymap my-emacs-super-keys-map)


;; end here
(provide 'my-emacs-hyper-super-keys)
