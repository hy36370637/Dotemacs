;;; 자주 사용하는 것 . Super key할당하여 전역적으로 minor mode
;;;  I was inspired by the https://protesilaos.com
  (defvar-keymap my-emacs-super-keys-map
    :doc "Common command alternatives using the Super key."
    "s-b" #'switch-to-buffer
    ;;  "s-e" #'eshell
    "s-g" #'consult-grep
    "s-d" #'consult-dir
    ;; "s-o" #'other-window
    "s-c" #'keycast-mode-line-mode;;
    "s-f" #'toggle-frame-fullscreen
    "s-r" #'toggle-streaming)

  (define-minor-mode my-emacs-super-keys-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap my-emacs-super-keys-map)

(provide 'my-emacs-super-keys)
