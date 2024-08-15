;;; -*- lexical-binding: t; -*-
;; ======================================
;;; view-mode
;; ======================================
;; 글씨 확대, 편집 보호
;; /.emacs.d/lisp/my-reading-mode-custom.el
(use-package view
  :ensure nil    ;built-in
  :init  (setq view-read-only t)
  :bind(:map view-mode-map
             ("n" . View-scroll-line-forward)
             ("p" . View-scroll-line-backward)))

;; ======================================
;;; my-view-mode
;; ======================================
;; "Read-only mode, not editable."
;; I drew inspiration from novel-mode and view-mode.
;; Therefore, you should refer to the view-mode settings.

(defvar my-view-mode-enabled nil
  "Non-nil if my-view-mode is enabled.")

(defun toggle-my-view-mode ()
  "Toggle fullscreen & view-mode."
  (interactive)
  (if my-view-mode-enabled
      (progn
        (toggle-frame-fullscreen)
        (text-scale-decrease 0.5)
        (setq my-view-mode-enabled nil)
        (view-mode -1))
    (progn
      (unless (eq (frame-parameter nil 'fullscreen) 'fullboth)
        (toggle-frame-fullscreen))
      (text-scale-increase 0.5)
      (setq my-view-mode-enabled t)
      (view-mode 1))))



;; end here
(provide 'my-view-mode-custom)
