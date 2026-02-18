;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-session.el


;; =======================================
;;; Bookmark
;; =======================================
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))


;; =======================================
;;; Register
;; =======================================
(use-package register
  :ensure nil
  :config
  (let ((org-dir my/org-person-dir)
	(conf-dir user-emacs-directory))
    (set-register ?i `(file . ,(expand-file-name "init.el" conf-dir)))
    (set-register ?l `(file . ,(expand-file-name "lisp/" conf-dir)))
    (set-register ?r `(file . ,(concat org-dir "cReading.org")))
    (set-register ?d `(file . ,(concat org-dir "Daily.org")))
    (set-register ?n `(file . ,(concat org-dir "cNotes.org")))
    (set-register ?p `(file . ,(expand-file-name "~/Dropbox/Docs/pdf"))))
  (set-register ?o `(file . ,default-directory))
  :custom
  (register-preview-delay 0.5))


;; =======================================
;;; Session and Place Persistence
;; =======================================
(use-package savehist
  :ensure nil
  :demand t
  :init (savehist-mode 1)
  :custom
  (history-length 10))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))


;; =======================================
;;; Manual Session Management
;; =======================================
(use-package desktop
  :ensure nil
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-save 'if-exists)
  (desktop-buffers-not-to-save "\\(^\\*\\|\\.log$\\)")
  (desktop-save-mode nil) 
  :config
  (defun my/desktop-save-at-point ()
    "Save all current buffers and window configurations."
    (interactive)
    (desktop-save user-emacs-directory)
    (message "✅ [Layout Saved] Current configuration has been recorded."))

  (defun my/desktop-read-at-point ()
    "Restore the saved desktop session."
    (interactive)
    (desktop-read user-emacs-directory)
    (message "✅ [Layout Restored] Previous session has been restored."))
  :bind
  (("C-x r S" . my/desktop-save-at-point)   ; Save Layout
   ("C-x r R" . my/desktop-read-at-point))) ; Restore Layout


;; =======================================
;;; Auto-revert (Dropbox Sync Optimization)
;; =======================================
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 60)         ; 60초 간격
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :hook (focus-in . (lambda ()
                      (when (fboundp 'auto-revert-buffers)
                        (auto-revert-buffers))))
  :config
  (global-auto-revert-mode t))



(provide 'my-session)
;; end here
