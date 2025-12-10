;;; my-dired-custom.el --- Custom Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired customization with Korean filename support and enhanced features
;; Note: (setenv "LC_COLLATE" "C") in init.el handles Korean filename sorting

;;; Code:

;; ======================================
;;; Dired
;; ======================================
(use-package dired
  :ensure nil
  :custom
  ;; Listing options
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  ;; Behavior
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-free-space nil)
  :bind
  (:map dired-mode-map
   ("C-+" . dired-create-empty-file)
   ("C-<return>" . dired-do-open)
   ("/" . dired-narrow)))

;; ======================================
;;; Dired Extensions
;; ======================================
(use-package dired-narrow
  :after dired)

;;; my-dired-custom.el ends here
(provide 'my-dired-custom)
