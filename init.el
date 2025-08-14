;; -*- lexical-binding: t -*-
;;  default Config for EMACS

;; =======================================
;; Global variables
;; =======================================
(defvar my/lisp-path (expand-file-name "lisp/" user-emacs-directory)
  "Path to the user's personal lisp directory.")

(defvar my/org-person-dir "~/Dropbox/Docs/Person/"
  "Directory for personal org files.")

;; =======================================
;;; Custom file
;; =======================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file t t)

;; =======================================
;;; Package initialization
;; =======================================
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; =======================================
;;; Configure use-package
;; =======================================
;; 패키지 자동 로딩 최적화
(setq package-install-upgrade-built-in nil
      package-quickstart t)  ; 패키지 빠른 시작 활성화

;; use-package 최적화
(setq use-package-always-ensure nil
      use-package-always-defer nil  
      use-package-expand-minimally t) ; 매크로 확장 최소화

;; =======================================
;;; System info
;; =======================================
(defvar my-mactop-p (eq system-type 'darwin))
(defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; =======================================
;;; exec-path-from-shell
;; =======================================
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer 2  ; 2초 후 로딩
  :config
  (exec-path-from-shell-initialize))

;; =======================================
;;; Load custom packages
;; =======================================
;; 사용자 Lisp 디렉토리를 load-path에 추가.
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; autoload할 파일 목록, 변수 정의.
(setq my-autoload-files '("my-web-search.el" "my-todays-pop.el"))

;; autoload 처리
(autoload 'my-custom-search-text "my-web-search" "macDic, Naver, 구글 or 나무위키, 날씨 검색." t)
(autoload 'my/naver-weather-search "my-web-search" "Naver 날씨." t)
;;(global-set-key (kbd "C-c p s") 'my-custom-search-text)

(autoload 'my-todays-pop "my-todays-pop" "오늘 정보 등" t)
;;(global-set-key (kbd "s-3") 'my-todays-pop)

;; my-autoload-files 변수에 지정된 파일을 제외한 나머지 .el 파일 로드.
(dolist (file (directory-files "~/.emacs.d/lisp" t "\\.el$"))
 (condition-case err
      (unless (member (file-name-nondirectory file) my-autoload-files)
        (load file))
    (error (message "Error loading %s: %s" file err))))

;; =======================================
;;; MacOS keyboard
;; =======================================
;; macOS 환경에서만 아래 설정을 적용
; alfred snippets 불능. OS시스템 설정에서 키보드 교환
;; (when my-mactop-p       
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-option-modifier 'super))

;; =======================================
;;; Emacs UI and behavior
;; =======================================
(use-package emacs
  :init
  (setq  default-directory (expand-file-name "~/Dropbox/Docs/org")
         temporary-file-directory "~/tmpdir/"
         kill-whole-line 1
         search-highlight t
         text-scale-mode-step 1.05  	;글꼴 확대축소 비율 5% 단위
	 display-time-format "%m.%d(%a) %H:%M") ; 원하는 날짜/시간 형식
  :config
  (setq-default line-spacing 0.2)
  (global-font-lock-mode 1)
  (global-visual-line-mode t)
  (global-auto-revert-mode 1)
  (transient-mark-mode t)
  (column-number-mode t)
  (display-time-mode 1))

(use-package emacs
  :bind
  ( :map global-map
    ("C-x f" . nil)
    ("C-x m". nil)
    ("C-x C-r" . restart-emacs)
    ("C-c p s" . my-custom-search-text)
    ("C-c p t" . my-todays-pop)
    ("C-c 0" . toggle-frame-fullscreen)
  ))

;; =======================================
;;; Bookmark
;; =======================================
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

;; =======================================
;;; Register
;; =======================================
(use-package register
  :ensure nil				;built-in
  :config
  (setq register-preview-delay 0
        register-preview-function #'register-preview-default)
  (set-register ?i '(file . "~/.emacs.d/init.el"))
  (set-register ?r `(file . ,(concat my/org-person-dir "cReading.org")))
  (set-register ?d `(file . ,(concat my/org-person-dir "Daily.org")))
  (set-register ?n `(file . ,(concat my/org-person-dir "cNotes.org")))
  :bind
  (("C-x r j" . jump-to-register)
   ("C-x r i" . insert-register)))

;; =======================================
;;; Locale and Korean settings
;; =======================================
(use-package emacs
  :config
  (setenv "LANG" "ko_KR.UTF-8")
  (setenv "LC_COLLATE" "C")
  (set-locale-environment "ko_KR.UTF-8")
  (setq default-input-method "korean-hangul"
	input-method-verbose-flag nil
	input-method-highlight-flag nil))

;; =======================================
;;; Fonts
;; =======================================
(use-package emacs
  :init
  ;; 폰트 캐시 압축 비활성화 (전역 적용)
  (setq inhibit-compacting-font-caches t)
  :config
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Noto Sans KR" :height 160)
    (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
    (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))))
;; =======================================
;;; Theme
;; =======================================
;; (use-package emacs
;;   :config
;;   (require-theme 'modus-themes)
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;; 	modus-themes-mode-line '(accented borderless padded))
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-intense)
;;  (load-theme 'modus-operandi-tinted))

;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))

;; =======================================
;;; Helpful
;; =======================================
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

;; =======================================
;;; Session and Place Persistence
;; =======================================
(use-package savehist
  :ensure nil
  :demand t
  :init (savehist-mode 1)
  :config
  (setq history-length 10))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))

;; =======================================
;;; Icons
;; =======================================
(use-package nerd-icons
  :ensure nil
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :ensure nil
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; =======================================
;;; recentF
;; =======================================
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 15)
  (setq recentf-max-saved-items 15))

;; =======================================
;;; hi-line
;; =======================================
;; (use-package hl-line
;;   :ensure nil
;;   :custom
;;   (hl-line-sticky-flag nil)
;;   :hook
;;   ((dired-mode text-mode emacs-lisp-mode) . hl-line-mode))

;; =======================================
;;; Eshell
;; =======================================
(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; =======================================
;;; Modeline
;; =======================================
(defvar ko-indicator (create-image "~/.emacs.d/img-indicator/han2.tiff" 'tiff nil :ascent 'center))
(defvar en-indicator (create-image "~/.emacs.d/img-indicator/qwerty.tiff" 'tiff nil :ascent 'center))
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
		;; (:eval (propertize
                ;;         (if (string= current-input-method "korean-hangul")
                ;;             "KO" "EN")))
		(:eval (propertize " "
				   'display
				   (if (string= current-input-method "korean-hangul")
				       ko-indicator
				     en-indicator)))
                "   "
                "Ⓗ "
                mode-line-buffer-identification
                mode-line-frame-identification
;;                " Ⓨ "
                mode-line-modes
                mode-line-format-right-align
                mode-line-position
		"Ⓨ "
                mode-line-misc-info))

;; =======================================
;;; Battery display
;; =======================================
(when my-Macbook-p
  (use-package battery
    :ensure nil
    :demand t  ; 즉시 로딩 강제
    :config
    (setq battery-status-function 'battery-pmset
          battery-mode-line-format "Ⓑ %p%% ")
    (display-battery-mode 1)))
