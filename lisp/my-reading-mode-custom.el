;;; -*- lexical-binding: t; -*-
;; ======================================
;;; view-mode
;; ======================================
;; 주로 org-mode를 이용한 읽기모드
;; /emacs/lisp/my-reading-mode-custom.el
;; 읽기 모드, 편집 보호

(use-package view
  :ensure nil    ;built-in
  :init
  (setq view-read-only t)
  :bind
  (:map view-mode-map
        ("n" . View-scroll-line-forward)
        ("p" . View-scroll-line-backward)))
;;
;; ======================================
;;; my-reading(view)-mode
;; ======================================
;; "Read-only mode, not editable."
;; I drew inspiration from novel-mode and view-mode.
;; Therefore, you should refer to the view-mode settings.
(defun toggle-my-reading-mode ()
  "Toggle fullscreen & view-mode."
  (interactive)
  (if (and (boundp 'my-reading-mode-enabled) my-reading-mode-enabled)
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
      (view-mode))))

(provide 'my-reading-mode-custom)
