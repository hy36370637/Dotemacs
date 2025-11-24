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
;; 1. 저장소 목록 정의
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; 2. 우선순위 설정 
(setq package-archive-priorities
      '(("nongnu" . 20)  ;; 1순위: 공식이면서 대중적인 패키지 (Magit 등)
        ("gnu-elpa" . 10)     ;; 2순위: Emacs 핵심 공식 패키지
        ("melpa" . 5)))  ;; 3순위: 위 두 곳에 없는 나머지 모든 패키지

;; 패키지 초기화
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
(defvar my-macOS-p (eq system-type 'darwin))
(defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; =======================================
;;; exec-path-from-shell
;; =======================================
  (use-package exec-path-from-shell
    :defer 2     ; 2초 후 로딩
    :config
    ;; shell에서 환경변수들을 가져올 변수명 지정
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LIBRARY_PATH"))
    ;; shell PATH를 Emacs로 가져오기
    (exec-path-from-shell-initialize))

;; =======================================
;;; Homebrew GCC & Native Comp 설정
;; =======================================
(when my-macOS-p
  ;; 네이티브 컴파일 경고 억제
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq warning-suppress-log-types '((comp) (bytecomp)))
  (setq warning-suppress-types '((comp) (bytecomp)))

  (let* ((homebrew-base "/opt/homebrew")
         ;; 1. GCC 바이너리 경로 (opt/gcc 사용)
         (gcc-base (concat homebrew-base "/opt/gcc"))
         (gcc-bin (concat gcc-base "/bin"))
         
         ;; 2. 버전 (15로 고정)
         (gcc-version "15")
         
         ;; 3. libgccjit 기본 경로
         (libgccjit-base (concat homebrew-base "/opt/libgccjit"))
         
         ;; [중요] 스크린샷 경로 반영: /lib/gcc/15 폴더 지정
         (libgccjit-lib (concat libgccjit-base "/lib/gcc/" gcc-version))
         (libgccjit-include (concat libgccjit-base "/include")))

    ;; GCC와 libgccjit 경로 확인
    (when (and (file-directory-p gcc-bin)
               (file-exists-p (concat libgccjit-lib "/libgccjit.dylib")))
      
      ;; PATH 설정
      (setq exec-path (cons gcc-bin exec-path))
      (setenv "PATH" (concat gcc-bin ":" (getenv "PATH")))
      
      ;; 컴파일러 지정
      (setenv "CC" (concat gcc-bin "/gcc-" gcc-version))
      (setenv "CXX" (concat gcc-bin "/g++-" gcc-version))
      
      ;; 라이브러리 경로 환경변수 설정
      (setenv "LIBGCCJIT_EXEC_PREFIX" (concat libgccjit-lib "/"))
      (setenv "LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "LIBRARY_PATH") "")))
      (setenv "LD_LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "LD_LIBRARY_PATH") "")))
      (setenv "DYLD_LIBRARY_PATH" (concat libgccjit-lib ":" (or (getenv "DYLD_LIBRARY_PATH") "")))
      
      ;; 옵션 설정
      (setq native-comp-driver-options nil)
      (setq native-comp-speed 2))))
      
      ;; 성공 메시지 (확인용)
;;      (message "✅ Native Compilation Configured (GCC-%s)" gcc-version))))

;; =======================================
;;; Dired & Path 설정 (gls 에러 해결)
;; =======================================
(when my-macOS-p
  ;; 1. Homebrew의 bin 폴더(/opt/homebrew/bin)를 Emacs 경로에 강제 추가
  (let ((brew-bin "/opt/homebrew/bin"))
    (when (file-directory-p brew-bin)
      (add-to-list 'exec-path brew-bin)
      (setenv "PATH" (concat brew-bin ":" (getenv "PATH")))))

  ;; 2. Dired에서 사용할 ls 프로그램 설정 (gls 우선)
  (let ((gls-prog (executable-find "gls")))
    (if gls-prog
        (progn
          (setq insert-directory-program gls-prog)
          (setq dired-use-ls-dired t))
      ;; gls가 없으면 기본 ls 사용
      (setq insert-directory-program "ls")
      (setq dired-use-ls-dired nil))))

;; =======================================
;;; TeX/XeLaTeX PATH 설정
;; =======================================
(when my-macOS-p
  ;; XeLaTeX 경로(/Library/TeX/texbin)를 Emacs 경로에 강제 추가
  (let ((tex-bin "/Library/TeX/texbin"))
    (when (file-directory-p tex-bin)
      (add-to-list 'exec-path tex-bin)
      (setenv "PATH" (concat tex-bin ":" (getenv "PATH"))))))


;; =======================================
;;; Load custom packages
;; =======================================
;; Emacs 24.4+ 에서 자동으로 최신 파일(.el vs .elc) 선택
(when (fboundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; 사용자 Lisp 디렉토리를 load-path에 추가
(add-to-list 'load-path my/lisp-path)

;; autoload할 파일 목록 (확장자 제외 - .el/.elc 자동 처리)
(setq my-autoload-files '("my-web-search" "my-todays-pop"))

;; autoload 처리 - Emacs가 자동으로 .el/.elc 선택
(autoload 'my-custom-search-text "my-web-search" "macDic, Naver, 구글 or 나무위키, 날씨 검색." t)
(autoload 'my/naver-weather-search "my-web-search" "Naver 날씨." t)
(autoload 'my-todays-pop "my-todays-pop" "오늘 정보 등" t)

(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (dolist (file (directory-files lisp-dir t "\\.el$"))
    (condition-case err
        (let ((base-name (file-name-sans-extension (file-name-nondirectory file))))
          (unless (member base-name my-autoload-files)
            ;; load 함수가 자동으로 .el/.elc 중 최신 파일 선택
            (load (file-name-sans-extension file) nil t)))
      (error (message "Error loading %s: %s" file err)))))

;; =======================================
;;; MacOS keyboard
;; =======================================
;; macOS 환경에서만 아래 설정을 적용
; alfred snippets 불능. OS시스템 설정에서 키보드 교환
(when my-macOS-p
   (setq mac-right-option-modifier 'none))


;; =======================================
;;; Emacs UI and behavior
;; =======================================
(use-package emacs
  :init
  (setq  default-directory (expand-file-name "~/Dropbox/Docs/org")
	 temporary-file-directory (expand-file-name "tmp/" user-emacs-directory)
         kill-whole-line 1
         search-highlight t
	 text-scale-mode-step 1.02)
  :config
  (setq-default line-spacing 0.2)
  (global-font-lock-mode 1)
  (global-visual-line-mode t)
  (global-auto-revert-mode 1)
  (transient-mark-mode t)
  (column-number-mode t)
  (display-time-mode t)
  :custom
  (use-short-answers t)   ;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
  )

(use-package time
  :ensure nil
;;  :hook (after-init . display-time-mode)
  :custom
  (display-time-24hr-format t)                         ;; 24-hour system
  (display-time-format "%y-%m-%d (%a) %H:%M")
  (display-time-day-and-date t))                     ;; Show time, day, date

(use-package emacs
  :bind
  ( :map global-map
    ("C-x f" . nil)
    ("C-x m". nil)
    ("C-x z". nil)
    ("<f8>" . repeat)
    ("C-c n s" . my-custom-search-text)
    ("C-c n t" . my-todays-pop)
    ("C-c 0" . toggle-frame-fullscreen)))

;; =======================================
;;; Bookmark
;; =======================================
(use-package bookmark
  :ensure nil                                     ;; built in
  :custom
  (bookmark-save-flag 1)                ;; 변경 때마다 자동 저장
  (bookmark-sort-flag nil)              ;; 정렬 안 함(입력 순서 유지)
  (bookmark-default-file
   (expand-file-name "bookmarks" user-emacs-directory)))

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
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-mode-line '(accented borderless padded))
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)
 (load-theme 'modus-operandi-tinted))

;;(load-theme 'tango-dark t)
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
   ("C-h C" . helpful-command)
   ))

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
  :ensure t
  :if (display-graphic-p)
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font"))

(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1))

;; =======================================
;;; recentF
;; =======================================
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 15)
  (setq recentf-max-saved-items 15))

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
;; 이미지 아이콘 (GUI에서 사용)
(defvar ko-indicator (create-image "~/.emacs.d/img-indicator/han2.tiff" 'tiff nil :ascent 'center))
(defvar en-indicator (create-image "~/.emacs.d/img-indicator/qwerty.tiff" 'tiff nil :ascent 'center))
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
                ;; 입력기 상태: GUI=이미지+툴팁, 터미널=텍스트(“KO/EN”) + 툴팁
                (:eval
                 (let* ((is-ko (equal current-input-method "korean-hangul"))
                        (img    (if is-ko ko-indicator en-indicator))
                        (label  (if is-ko "KO" "EN"))
                        (tip    (if is-ko "KO" "EN"))
                        ;; GUI이며 해당 이미지 타입을 쓸 수 있을 때만 아이콘 표시
                        (use-img (and (display-graphic-p)
                                      (image-type-available-p 'tiff))))
                   (if use-img
                       ;; GUI: 이미지 아이콘(대체텍스트=label), 마우스오버 툴팁
                       (propertize label
                                   'display   img
                                   'help-echo tip)
                     ;; 터미널/이미지 불가: 텍스트 라벨로 직접 표시
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
    :demand t  ; 즉시 로딩 강제
    :config
    (setq battery-status-function 'battery-pmset
          battery-mode-line-format "Ⓑ %p%% ")
    (display-battery-mode 1)))
