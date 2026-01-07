;;; my-dired-custom.el --- Custom Dired configuration -*- lexical-binding: t; -*-

;; ======================================
;;; Dired Move Function
;; ======================================
(defun my-dired-move-to-pdf-folder-safe ()
  "Move marked files to '/pdf' safely."
  (interactive)
  (let* ((target-dir "../pdf/")
         (expanded-target (expand-file-name target-dir))
         (files (dired-get-marked-files)))
    (unless (file-exists-p expanded-target)
      (make-directory expanded-target t))
    (condition-case nil
        (progn
          (dired-do-rename-regexp ".*" expanded-target nil t)
          (revert-buffer)
          (message "%d files moved to %s." (length files) target-dir))
      (quit (message "Move canceled."))
      (error (message "Error occurred during move.")))))

;; ======================================
;;; Dired Main Configuration
;; ======================================
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-free-space nil)
  :bind
  (:map dired-mode-map
   ("C-<return>" . dired-do-open)
   ("/" . dired-narrow)
   ("M" . my-dired-move-to-pdf-folder-safe)))

;; ======================================
;;; Dired Extensions
;; ======================================
(use-package dired-narrow
  :after dired)

(provide 'my-dired-custom)
