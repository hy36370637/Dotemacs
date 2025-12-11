;; -*- lexical-binding: t -*-
;;  emacs for macOS
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

;; 저장소 목록 및 우선순위 설정
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("nongnu" . 20)
        ("gnu-elpa" . 10)
        ("melpa-stable" . 5)))

;; 패키지 초기화 및 최적화
(setq package-install-upgrade-built-in nil
      package-quickstart t
      use-package-always-ensure nil
      use-package-always-defer nil
      use-package-expand-minimally t)

(package-initialize)

;; =======================================
;;; System info
;; =======================================
(defvar my-macOS-p (eq system-type 'darwin))
(defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; =======================================
;;; exec-path-from-shell
;; =======================================
(use-package exec-path-from-shell
  :defer 2
  :if my-macOS-p
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LIBRARY_PATH"))
  (exec-path-from-shell-initialize))

;; =======================================
;;; macOS PATH
;; =======================================
(when my-macOS-p
  (let ((brew-bin "/opt/homebrew/bin")
        (tex-bin "/Library/TeX/texbin"))
    (dolist (path (list brew-bin tex-bin))
      (when (file-directory-p path)
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat path ":" (getenv "PATH")))))
    
    ;; Dired ls 설정
    (let ((gls-prog (executable-find "gls")))
      (setq insert-directory-program (or gls-prog "ls")
            dired-use-ls-dired (if gls-prog t nil)))))

;; =======================================
;;; Load custom packages
;; =======================================
(add-to-list 'load-path my/lisp-path)
;; Autoload
;;(autoload 'my-search-text-in-range "my-search" "macDic, Naver, 구글 or 나무위키" t)
(autoload 'my-weather-search "my-search" "Naver 날씨." t)
(autoload 'my-todays-pop "my-todays-pop" "오늘 정보 등" t)
(require 'my-completion)
(require 'my-calendar)
(require 'my-eradio-custom)
(require 'my-dired-custom)
(require 'my-org-custom)
(require 'my-useful-custom)
(require 'my-search)
;; =======================================
;;; MacOS keyboard
;; =======================================
; alfred snippets 불능. OS시스템 설정에서 키보드 교환
(when my-macOS-p
   (setq mac-right-option-modifier 'none))

;; =======================================
;;; Emacs UI and behavior
;; =======================================
(use-package emacs
  :init
  (setq default-directory (expand-file-name "~/Dropbox/Docs/org")
        temporary-file-directory (expand-file-name "tmp/" user-emacs-directory))
  :hook
  (text-mode . visual-line-mode)
  :custom
  (use-short-answers t)
  (kill-whole-line 1)
  (text-scale-mode-step 1.02)
  (line-spacing 0.2)
  (global-auto-revert-mode t)
  (column-number-mode t)
  (display-time-mode t)
  :config
  (minibuffer-depth-indicate-mode 1) ;; 미니버퍼 재귀 깊이 표시모드 for consult-dir
  :bind
  (("C-x f" . nil)
   ("C-x m" . nil)
   ("C-x z" . nil)
   ("C-c n s" . my-search-text-in-range)
   ("C-c n t" . my-todays-pop)
   ("C-c n w" . my-weather-search)
   ("C-c 0" . toggle-frame-fullscreen)))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)                         ;; 24-hour system
  (display-time-format "%y-%m-%d (%a) %H:%M")
  (display-time-day-and-date t)
  (display-time-load-average nil))                  ;mode-line-misc-info average nil

;; ========================================================
;; Window Management (Macbook Air 13")
;; ========================================================
(when my-Macbook-p
  (setq split-window-preferred-direction 'horizontal
        window-combination-resize t
        even-window-sizes 'height-only
        window-sides-vertical nil
        switch-to-buffer-in-dedicated-window 'pop
        split-height-threshold 35
        split-width-threshold 85
        window-min-height 3
        window-min-width 30))

;; =======================================
;;; Bookmark
;; =======================================
(use-package bookmark
  :ensure nil                                     ;; built in
  :custom
  (bookmark-save-flag 1)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))

;; =======================================
;;; Register
;; =======================================
(use-package register
  :ensure nil				;; built-in
  :config
  (setq register-preview-delay 0
        register-preview-function #'register-preview-default)
  (set-register ?i `(file . ,(expand-file-name "init.el" user-emacs-directory)))
  (set-register ?r `(file . ,(concat my/org-person-dir "cReading.org")))
  (set-register ?d `(file . ,(concat my/org-person-dir "Daily.org")))
  (set-register ?n `(file . ,(concat my/org-person-dir "cNotes.org"))))

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
  :if (display-graphic-p)
  :init
  (setq inhibit-compacting-font-caches t)
  :config
  (set-face-attribute 'default nil :family "Noto Sans KR" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR")))

;; =======================================
;;; Theme
;; =======================================
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-mode-line '(accented borderless padded))
 (load-theme 'modus-operandi-tinted))

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
  :if (display-graphic-p)
  :custom (nerd-icons-font-family "Symbols Nerd Font"))

(use-package nerd-icons-dired
  :if (display-graphic-p)
;;  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after (marginalia nerd-icons)
  :config  (nerd-icons-completion-mode 1))

;; =======================================
;;; recentF
;; =======================================
(use-package recentf
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 15))

;; =======================================
;;; Eshell
;; =======================================
(use-package eshell
  :defer t
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
                (:eval
                 (let* ((is-ko (equal current-input-method "korean-hangul"))
                        (img    (if is-ko ko-indicator en-indicator))
                        (label  (if is-ko "KO" "EN"))
                        (tip    (if is-ko "KO" "EN"))
                        (use-img (and (display-graphic-p)
                                      (image-type-available-p 'tiff))))
                   (if use-img
                       (propertize label
                                   'display   img
                                   'help-echo tip)
                     (propertize label
                                 'help-echo tip))))
                "   "
                "Ⓗ "
                mode-line-buffer-identification
                mode-line-frame-identification
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
    :demand t
    :config
    (setq battery-status-function 'battery-pmset
          battery-mode-line-format "Ⓑ %p%% ")
    (display-battery-mode 1)))

;; =======================================
;;; Homebrew GCC & Native Comp 설정
;; =======================================
;; (when my-macOS-p
;;   ;; 네이티브 컴파일 경고 억제
;;   (setq native-comp-async-report-warnings-errors 'silent)
;;   (setq warning-suppress-log-types '((comp) (bytecomp)))
;;   (setq warning-suppress-types '((comp) (bytecomp)))

;;   (let* ((homebrew-base "/opt/homebrew")
;;          ;; 1. GCC 바이너리 경로 (opt/gcc 사용)
;;          (gcc-base (concat homebrew-base "/opt/gcc"))
;;          (gcc-bin (concat gcc-base "/bin"))
         
;;          ;; 2. 버전 (15로 고정)
;;          (gcc-version "15")
         
;;          ;; 3. libgccjit 기본 경로
;;          (libgccjit-base (concat homebrew-base "/opt/libgccjit"))
         
;;          ;; [중요] 스크린샷 경로 반영: /lib/gcc/15 폴더 지정
;;          (libgccjit-lib (concat libgccjit-base "/lib/gcc/" gcc-version))
;;          (libgccjit-include (concat libgccjit-base "/include")))

;;     ;; GCC와 libgccjit 경로 확인
;;     (when (and (file-directory-p gcc-bin)
;;                (file-exists-p (concat libgccjit-lib "/libgccjit.dylib")))
      
;;       ;; PATH 설정
;;       (setq exec-path (cons gcc-bin exec-path))
;;       (setenv "PATH" (concat gcc-bin ":" (getenv "PATH")))
      
;;       ;; 컴파일러 지정
;;       (setenv "CC" (concat gcc-bin "/gcc-" gcc-version))
;;       (setenv "CXX" (concat gcc-bin "/g++-" gcc-version))
      
;;       ;; 라이브러리 경로 환경변수 설정
;;       (setenv "LIBGCCJIT_EXEC_PREFIX" (concat libgccjit-lib "/"))
;;       (setenv "LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "LIBRARY_PATH") "")))
;;       (setenv "LD_LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "LD_LIBRARY_PATH") "")))
;;       (setenv "DYLD_LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "DYLD_LIBRARY_PATH") "")))
      
;;       ;; 옵션 설정
;;       (setq native-comp-driver-options nil)
;;       (setq native-comp-speed 2))))
      
;;       ;; 성공 메시지 (확인용)
;; ;;      (message "✅ Native Compilation Configured (GCC-%s)" gcc-version))))
