;;; -*- lexical-binding: t; -*-
;;; 자주 사용 → Super key 할당 → minor mode
;;;  I was inspired by the https://protesilaos.com
;;; unset-super-key
(global-unset-key (kbd "s-t"))    ; set font
(global-unset-key (kbd "s-c"))    ; Copy
(global-unset-key (kbd "s-e"))    ; isearch-yank-kill
(global-unset-key (kbd "s-f"))     ; I-search forward
(global-unset-key (kbd "s-g"))    ;
(global-unset-key (kbd "s-o"))    ;
(global-unset-key (kbd "s-p"))    ; ns-print-buffer
(global-unset-key (kbd "s-l"))     ; goto-line
(global-unset-key (kbd "s-m"))     ; 
(global-unset-key (kbd "s-n"))    ; make-frame
(global-unset-key (kbd "s-x"))     ; kill-region
(global-unset-key (kbd "s-z"))     ; undo(C-/, C-x u)
;; "s-w" delete-frame, "s-q" save-buffers-kill-emacs, "s-t" unbind, "s-y" ns-paste-secondary, "s-u" revert-buffer, "s-i" unbind, "s-o" other-window, "s-p" ns-print-buffer, "s-a" mark-whole-buffer, "s-s" save-buffer, "s-d" consult-dir, "s-f" toggle-frame-fullscreen, "s-g" consult-grep, "s-;" unbind, "s-'" next-window-any-frame, "s-z" unbind, "s-x" unbind,  "s-v" yank, "s-b" switch-to-buffer, "s-n" make-frame, "s-m" my-prefix-map, "s-," unbind, "s-." unbind, "s-/" undo-redo

  (defvar-keymap my-emacs-super-keys-map
    :doc "Common command alternatives using the Super key."
    ;; "s-a" #'mark-whole-buffer
    "s-b" #'switch-to-buffer
    "s-c" #'org-capture; #'keycast-mode-line-mode
    "s-d" #'consult-dir
    "s-e" #'eshell
    "s-f" #'toggle-frame-fullscreen    
    "s-g" #'consult-grep
    ;; "s-h" #'ns-do-hide-emacs   ;창 숨기기
    ;; "s-j" #'exchange-point-and-mark(C-x C-x)
    ;; "s-k" #'kill-current-buffer
    "s-l" #'my-org-latex-custom  ;goto-line
    "s-m" #'modus-themes-toggle
    "s-n" #'find-file
    ;; "s-o" #'org-agenda
    "s-p" #'my-popmark    ;#'ns-print-buffer
    ;; "s-q" #'save-buffers-kill-emacs  ;quit
    "s-r" #'toggle-streaming
    "s-z" #'repeat
    "s-/" #'undo-redo)

  (define-minor-mode my-emacs-super-keys-mode  ; minor-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap my-emacs-super-keys-map)

(provide 'my-emacs-super-keys)
