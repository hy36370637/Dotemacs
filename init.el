;; ======================================
;;; Config for EMACS
;; ======================================
;; by HO-YOUNG KIM(hy36370637)

;; ======================================
;;; Speed up emacs
;; ======================================
(use-package emacs
  :init
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 1024 1024 20)))) ; 20MB
  
  (defun my/set-gc-threshold ()
    "GC 임계값을 기본값으로 재설정합니다."
    (setq gc-cons-threshold (* 1024 1024 2))) ; 2MB  
  (add-hook 'focus-out-hook #'garbage-collect)
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil t))

;; ======================================
;;; package source list
;; ======================================
(use-package package
  :config
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize))

;; ======================================
;;; use-package
;; ======================================
(eval-when-compile
  (require 'use-package))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ======================================
;;; system-info
;; ======================================
(defvar my-mactop-p (eq system-type 'darwin))
(defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; ======================================
;;; exec-path-from-shell
;; ======================================
(use-package exec-path-from-shell
  :ensure t
;;  :if my-mactop-p
  :config
  (exec-path-from-shell-initialize))

;; ======================================
;;; load-my-custom-package
;; ======================================
(use-package load-dir
  :ensure t
  :config
  (setq load-dir-recursive t)
  (load-dir-one "~/.emacs.d/lisp/"))

;; ======================================
;;; MacOS keyboard
;; ======================================
(use-package emacs
 ;; :if my-mactop-p
  :config
  (setq ns-function-modifier 'hyper))

;; ======================================
;;; Emacs UI and behavior
;; ======================================
(use-package emacs
  :init
  (setq inhibit-startup-message t
        visible-bell t
        initial-scratch-message nil
        use-dialog-box nil
        default-directory "~/Dropbox/Docs/org/"
        temporary-file-directory "~/Dropbox/Docs/tmpdir/"
        make-backup-files nil
        kill-whole-line 1
        search-highlight t
        scroll-margin 7
        scroll-preserve-screen-position t
        scroll-conservatively 101
	text-scale-mode-step 1.05)	;글자크기 비율 5% 단위. 기본값 1.20
  :config
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (setq-default line-spacing 0.2)
  (save-place-mode 1)
  (global-font-lock-mode 1)
  (global-visual-line-mode t)
  (global-auto-revert-mode 1)
  (transient-mark-mode t)
  (column-number-mode t)
  (display-time-mode 1))

;; ======================================
;;; theme
;; ======================================
(use-package standard-themes
  :ensure t
  :config
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-disable-other-themes t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t
        standard-themes-prompts '(extrabold italic)
        standard-themes-headings
        '((0 . (variable-pitch light 1.3))
          (1 . (variable-pitch light 1.2))
          (2 . (variable-pitch light 1.2))
          (3 . (variable-pitch semilight 1.2))
          (4 . (variable-pitch semilight 1.2))
          (5 . (variable-pitch 1.2))
          (6 . (variable-pitch 1.2))
          (7 . (variable-pitch 1.2))
          (agenda-date . (1.1))
          (agenda-structure . (variable-pitch light 1.6))
          (t . (variable-pitch 1.1))))
  (standard-themes-load-dark))

;; ======================================
;;; Key bindings
;; ======================================
(use-package emacs
  :bind
  (([f11] . nil)
   ("C-x o" . nil)
   ("M-o" . other-window)))

;; ======================================
;;; Locale and Korean settings
;; ======================================
(use-package emacs
  :config
  (set-language-environment "Korean")
  (set-locale-environment "ko_KR.UTF-8")
  (setenv "LANG" "ko_KR.UTF-8")
  (setenv "LC_COLLATE" "C")
  (setq default-input-method "korean-hangul"
        input-method-verbose-flag nil
        input-method-highlight-flag nil))

;; ======================================
;;; Fonts
;; ======================================
(use-package emacs
;;  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil
                      :family "Noto Sans CJK KR"
                      :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans CJK KR")
  (set-fontset-font nil 'hangul (font-spec :family "Noto Sans CJK KR")))

;; ======================================
;;; helpful
;; ======================================
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

;; ======================================
;;; recentf
;; ======================================
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-saved-items 20))

;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure nil
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

;; ======================================
;;; marginalia
;; ======================================
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; ======================================
;;; savehist
;; ======================================
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; ======================================
;;; orderless
;; ======================================
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ======================================
;;; consult
;; ======================================
(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x r x" . consult-register)
   ("C-x r b" . consult-bookmark)
   ("C-c k" . consult-kmacro)
   ("M-g o" . consult-outline)
   ;; ("M-g h" . consult-org-heading)
   ;; ("M-g a" . consult-org-agenda)
   ("M-g m" . consult-mark)
   ("M-s f" . consult-find)
   ("M-s g" . consult-grep)
   ("M-s t" . consult-theme))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; ======================================
;;; consult-dir
;; ======================================
(use-package consult-dir
  :ensure t
  :after vertico consult
  :bind (("C-x c-d" . consult-dir)
          :map vertico-map
         ("C-x c-d" . consult-dir)
	 ("C-x c-j" . consult-dir-jump-file)))

;; ======================================
;;; embark
;; ======================================
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; ======================================
;;; embark-consult
;; ======================================
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ======================================
;;; Icons
;; ======================================
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; ======================================
;;; eshell
;; ======================================
(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; ======================================
;;; modeline
;; ======================================
(use-package emacs
  :init
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
		 (:eval (if (string= current-input-method "korean-hangul")
                             (propertize "KO" 'face '(:foreground "orange"))
                           "EN"))
                " Ⓗ "
                mode-line-buffer-identification       
                mode-line-frame-identification
                " Ⓨ "
                mode-line-modes
                mode-line-format-right-align
                mode-line-position
                " Ⓚ "
                mode-line-misc-info)))

;; ======================================
;;; keycast
;; ======================================
(use-package keycast
  :ensure t
  :config
  (setq keycast-mode-line-insert-after 'mode-line-modes
        keycast-mode-line-window-predicate 'mode-line-window-selected-p
        keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-mode -1))

;; ======================================
;;; battery display
;; ======================================
(use-package battery
  :if my-Macbook-p
  :config 
  (setq battery-status-function 'battery-pmset
        battery-mode-line-format "Ⓑ %p%%  ")
  (display-battery-mode 1))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))
