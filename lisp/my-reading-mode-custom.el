;;; -*- lexical-binding: t; -*-
;; ======================================
;;; view-mode
;; ======================================
;; 글씨 확대, 편집 보호
;; /emacs/lisp/my-reading-mode-custom.el

(use-package view
  :ensure nil    ;built-in
  :init
  (setq view-read-only t)
  :bind
  (:map view-mode-map
        ("n" . View-scroll-line-forward)
        ("p" . View-scroll-line-backward)))

;; ======================================
;;; my-reading(view)-mode
;; ======================================
;; "Read-only mode, not editable."
;; I drew inspiration from novel-mode and view-mode.
;; Therefore, you should refer to the view-mode settings.

(defvar my-reading-mode-enabled nil
  "Non-nil if my-reading-mode is enabled.")

(defun toggle-my-reading-mode ()
  "Toggle fullscreen & view-mode."
  (interactive)
  (if my-reading-mode-enabled
      (progn
        (toggle-frame-fullscreen)
        (text-scale-decrease 0.5)
        (setq my-reading-mode-enabled nil)
        (view-mode -1))
    (progn
      (unless (eq (frame-parameter nil 'fullscreen) 'fullboth)
        (toggle-frame-fullscreen))
      (text-scale-increase 0.5)
      (setq my-reading-mode-enabled t)
      (view-mode 1))))



;; end here
(provide 'my-reading-mode-custom)
