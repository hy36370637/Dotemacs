;; -*- lexical-binding: t -*-
;;; Config for EMACS

;; ======================================
;;; Speed up Emacs startup
;; ======================================
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 16))))  ; 16MB
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; ======================================
;;; Custom file
;; ======================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file t t)

;; ======================================
;;; Package initialization
;; ======================================
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; ======================================
;; Configure use-package
;; ======================================
(setq package-install-upgrade-built-in nil)         ; Prevent upgrading built-in packages
(require 'use-package)			                        ;emacs 27+
(setq use-package-always-ensure nil)	                ;emacs 29+

;; ======================================
;;; System info
;; ======================================
(defvar my-mactop-p (eq system-type 'darwin))
(defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; ======================================
;;; exec-path-from-shell
;; ======================================
(use-package exec-path-from-shell
;;  :if my-mactop-p
  :config
  (exec-path-from-shell-initialize))

;; ======================================
;;; Load custom packages
;; ======================================
(dolist (file (directory-files "~/.emacs.d/lisp" t "\\.el$"))
  (condition-case err
      (load file)
    (error (message "Error loading %s: %s" file err))))

;; ======================================
;;; MacOS keyboard
;; ======================================
;;(when my-mactop-p
;;  (setq ns-left-command-modifier nil))

;; ======================================
;;; Emacs UI and behavior
;; ======================================
(use-package emacs
  :hook (emacs-startup . my-fancy-startup-screen) ;my-useful-custom.el
  :init
  (setq inhibit-startup-message t
        visible-bell t
        initial-scratch-message nil
        use-dialog-box nil
        default-directory (expand-file-name "~/Dropbox/Docs/org")
        temporary-file-directory "~/tmpdir/"
        make-backup-files nil
        kill-whole-line 1
        search-highlight t
        scroll-margin 7
        scroll-preserve-screen-position t
        scroll-conservatively 101
        text-scale-mode-step 1.05)  	;글꼴 확대축소 비율 5% 단위
  :config
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (setq-default line-spacing 0.2)
  (global-font-lock-mode 1)
  (global-visual-line-mode t)
  (global-auto-revert-mode 1)
  (delete-selection-mode t)
  (transient-mark-mode t)
  (column-number-mode t)
  (display-time-mode 1))


;; ======================================
;;; Bookmark
;; ======================================
(use-package bookmark
  :ensure nil				;built-in
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :init
  (setq bookmark-save-flag 1
        bookmark-sort-flag nil
        bookmark-default-file "~/.emacs.d/bookmarks")
  :config
  (defun my/bookmark-save-automatically (&rest _)
    (when (boundp 'bookmark-alist)
      (bookmark-save)))
  (advice-add 'bookmark-set :after #'my/bookmark-save-automatically)
  :bind
  (("C-x r m" . bookmark-set)
   ("C-x r b" . bookmark-jump)
   ("C-x r l" . bookmark-bmenu-list)))

;; ======================================
;;; Register
;; ======================================
(use-package register
  :ensure nil				;built-in
  :config
  (setq register-preview-delay 0
        register-preview-function #'register-preview-default)
  :bind
  (("C-x r j" . jump-to-register)
;;   ("C-x r s" . copy-to-register)
   ("C-x r i" . insert-register)))
(set-register ?i '(file . "~/.emacs.d/init.el"))

;; ======================================
;;; Key bindings
;; ======================================
(use-package emacs
  :bind
  (([f11] . nil)
   ("C-x f" . nil)
   ("C-x m". nil)
   ("C-x o" . nil)
   ("M-o" . other-window)))

;; ======================================
;;; Locale and Korean settings
;; ======================================
(use-package emacs
  :config
  (setenv "LANG" "ko_KR.UTF-8")
  (setenv "LC_COLLATE" "C")
;;  (set-language-environment "Korean")    ;for linux
  (set-locale-environment "ko_KR.UTF-8")
  (setq default-input-method "korean-hangul"
	input-method-verbose-flag nil
	input-method-highlight-flag nil))

;; ======================================
;;; Fonts
;; ======================================
(use-package emacs
  :config
  (set-face-attribute 'default nil :family "Noto Sans KR" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans KR")
  (set-fontset-font nil 'hangul (font-spec :family "Noto Sans KR")))

;; ======================================
;;; Theme
;; ======================================
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)
  (load-theme 'modus-operandi-tinted))

;; ======================================
;;; Helpful
;; ======================================
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

;; ======================================
;;; Session and Place Persistence
;; ======================================
(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))

;; ======================================
;;; Icons
;; ======================================
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; =======================================
;;; hi-line
;; ======================================-
(use-package hl-line
  :ensure nil
  :custom
  (hl-line-sticky-flag nil)
  :hook
  (dired-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (emacs-lisp-mode . hl-line-mode))

;; ======================================
;;; Eshell
;; ======================================
(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; ======================================
;;; Modeline
;; ======================================
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
		(:eval (propertize
                        (if (string= current-input-method "korean-hangul")
                            "KO" "EN")))
                " Ⓗ "
                mode-line-buffer-identification
                mode-line-frame-identification
                " Ⓨ "
                mode-line-modes
                mode-line-format-right-align
                mode-line-position
                " Ⓚ "
                mode-line-misc-info))

;; ======================================
;;; Battery display
;; ======================================
(when my-Macbook-p
  (use-package battery
    :config
    (setq battery-status-function 'battery-pmset
          battery-mode-line-format "Ⓑ %p%%  ")
    (display-battery-mode 1)))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))

;; ======================================
;;; Keycast
;; ======================================
;; (use-package keycast
;;   :bind ("C-x m k" . keycast-mode-line-mode)
;;   :config
;;   (setq keycast-mode-line-insert-after 'mode-line-modes
;;         keycast-mode-line-window-predicate 'mode-line-window-selected-p
;;         keycast-mode-line-remove-tail-elemenets nil)
;;   (keycast-mode-line-mode -1))
