;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-app.el


;; =======================================
;;; recentF
;; =======================================
(use-package recentf
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 15))


;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure nil
  :init (which-key-mode)
  :custom
  (which-key-show-transient-maps t)
  (which-key-idle-delay 0.2))


;; =======================================
;;; Eshell
;; =======================================
(use-package eshell
  :defer t
  :custom
  (eshell-destroy-buffer-when-process-dies t))


;; =======================================
;;; Battery display
;; =======================================
(use-package battery
  :if my-Macbook-p
  :ensure nil
  :demand t
  :custom
  (battery-status-function 'battery-pmset)
  (battery-mode-line-format "Ⓑ %p%% ")
  :init
  (display-battery-mode 1))


;; =======================================
;;; magit
;; =======================================
(use-package magit
  :if my-Macbook-p
  :ensure nil
  :bind ("C-x g" . magit-status)
  :custom
  ;; Magit이 전체 화면을 차지하지 않고, 현재 창 구성을 최대한 유지
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; =======================================
;;; view-mode
;; =======================================
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


(provide 'my-app)
;;; my-app.el ends here
