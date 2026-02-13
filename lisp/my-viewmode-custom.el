;;; my-viewmode-custom.el --- configuration -*- lexical-binding: t; -*-
;; Enable read-only protection when entering view-mode

(use-package view
  :ensure nil
  :bind
  (:map view-mode-map
        ("n" . scroll-up-line)    ; Scroll screen down (text up)
        ("p" . scroll-down-line)  ; Scroll screen up (text down)
        ("e" . my-view-mode-edit-instantly))
  :hook (view-mode . my-view-mode-visual-setup)
  :custom
  (view-read-only t)) ; Enable read-only protection automatically

(defun my-view-mode-visual-setup ()
  "Toggle cursor shape and line highlighting for `view-mode`."
  (if view-mode
      (progn
        (hl-line-mode 1)
        ;; Read-only mode: Subtle horizontal bar cursor (1px)
        (setq-local cursor-type '(hbar . 1)))
    (progn
      (hl-line-mode -1)
      ;; Restore default cursor by killing the local variable
      (kill-local-variable 'cursor-type))))

(defun my-view-mode-edit-instantly ()
  "Disable `view-mode` and switch to edit mode immediately."
  (interactive)
  (when view-mode
    (view-mode -1)
    (message "📝 Switched to Edit Mode")))


(provide 'my-viewmode-custom)
;;; my-viewmode-custom.el ends here
