;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-winui.el


;; ========================================================
;; Window Management
;; ========================================================
(use-package emacs
  :if my-Macbook-p
  :custom
  (split-window-preferred-direction 'horizontal)
  (window-combination-resize t)
  (even-window-sizes 'height-only)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (split-height-threshold 35)    ; 세로가 35줄 이하이면 세로 분할 안함
  (split-width-threshold 85)     ; 가로가 85자 이상이면 가로 분할 선호
  (window-min-height 3)
  (window-min-width 30))


;; =======================================
;;; Fonts
;; =======================================  
(use-package emacs
  ;; :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil :family "Menlo" :height 180)
  (set-face-attribute 'fixed-pitch nil :family "Menlo" :height 1.0)
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans Mono CJK KR"))
  (set-face-attribute 'variable-pitch nil :family "Noto Sans CJK KR" :height 1.0)
  (set-fontset-font "fontset-default" 'hangul 
                    (font-spec :family "Noto Sans CJK KR") 
                    nil 'prepend)
  (setq face-font-rescale-alist '(("Noto Sans Mono CJK KR" . 0.95)
                                  ("Noto Sans CJK KR" . 0.95)))

  (add-hook 'org-mode-hook
            (lambda ()
              (variable-pitch-mode 1)
              (mapc (lambda (face)
                      (set-face-attribute face nil :inherit 'fixed-pitch))
                    '(org-table
                      org-code 
                      org-block 
                      org-block-begin-line 
                      org-block-end-line 
                      org-checkbox 
                      org-date 
                      org-link)))))


;; =======================================
;;; Theme
;; =======================================
(use-package modus-themes
  :ensure nil)

(use-package ef-themes
  :ensure t
  :demand t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>"   . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-symbiosis))


;; =======================================
;;; Icons
;; =======================================
(use-package nerd-icons
  ;; :if (display-graphic-p)
  :custom (nerd-icons-font-family "Symbols Nerd Font"))

(use-package nerd-icons-dired
  ;; :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  ;; :if (display-graphic-p)
  :after (marginalia nerd-icons)
  :config  (nerd-icons-completion-mode 1))


;; =======================================
;;; windmove
;; =======================================
(use-package windmove
  :ensure nil   ;built-in
  :bind
  (("C-x j" . windmove-left)
   ("C-x l" . windmove-right)
   ("C-x i" . windmove-up)
   ("C-x m" . windmove-down)))


;; =======================================
;;; winner
;; =======================================
(use-package winner
  :ensure nil    ;built-in
  :init
  (winner-mode 1))




(provide 'my-winui)
;; end here
