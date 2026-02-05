;;; my-viewmode-custom.el --- configuration -*- lexical-binding: t; -*-

;; Enable read-only protection when entering view-mode
(setq view-read-only t) 

(defun my-view-mode-edit-instantly ()
  "Disable view-mode immediately and switch to edit mode."
  (interactive)
  (when view-mode
    (view-mode -1)
    (message "Switched to Edit Mode")))

;; View-mode Configuration
(with-eval-after-load 'view
  ;; Assign 'e' key for instant transition to editing
  (define-key view-mode-map (kbd "e") 'my-view-mode-edit-instantly))

;; Visual enhancements when toggling view-mode
(add-hook 'view-mode-hook
          (lambda ()
            (if view-mode
                (progn
                  (hl-line-mode 1)               ; Enable line highlighting
                  (setq-local cursor-type 'bar))  ; Change cursor to a bar for reading
	      	  ;; (set-face-background 'hl-line (face-background color-lighten)))
              (hl-line-mode -1)                 ; Disable line highlighting
              (setq-local cursor-type 'box))))   ; Restore box cursor for editing




(provide 'my-viewmode-custom)
;;; my-viewmode-custom.el ends here
