;;; my-dired-custom.el --- Custom Dired configuration -*- lexical-binding: t; -*-

;; ======================================
;;; Dired Move Function
;; ======================================
;; (defun my-dired-move-to-pdf-folder-safe ()
;;   "Move marked files to '/pdf' safely."
;;   (interactive)
;;   (let* ((target-dir "../pdf/")
;;          (expanded-target (expand-file-name target-dir))
;;          (files (dired-get-marked-files)))
;;     (unless (file-exists-p expanded-target)
;;       (make-directory expanded-target t))
;;     (condition-case nil
;;         (progn
;;           ;; dired-do-rename-regexp는 복잡할 수 있으므로 
;;           ;; 단순히 마킹된 파일을 옮기려면 dired-do-rename을 고려
;;           (dired-do-rename-regexp ".*" expanded-target nil t)
;;           (revert-buffer)
;;           (message "%d files moved to %s." (length files) target-dir))
;;       (quit (message "Move canceled."))
;;       (error (message "Error occurred during move.")))))

(defun my-dired-move-to-pdf-folder-safe ()
  "Move marked files to '/pdf' safely."
  (interactive)
  (let* ((target-dir "../pdf/")
         (expanded-target (expand-file-name target-dir))
         (files (dired-get-marked-files)))
    (unless (file-exists-p expanded-target)
      (make-directory expanded-target t))
    (condition-case err
        (progn
          ;; 각 파일을 target 디렉토리로 이동
          (dolist (file files)
            (let ((target-file (expand-file-name 
                                (file-name-nondirectory file) 
                                expanded-target)))
              (rename-file file target-file)))
          (revert-buffer)
          (message "%d file(s) moved to %s." (length files) target-dir))
      (quit (message "Move canceled."))
      (error (message "Error occurred: %s" (error-message-string err))))))

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
   ("M" . my-dired-move-to-pdf-folder-safe)
   ("C-c f" . consult-focus-lines)))


(provide 'my-dired-custom)
