;; -*- lexical-binding: t -*-
;;  emacs for macOS
;; =======================================
;; Global variables
;; =======================================
(defvar my/lisp-path (expand-file-name "lisp/" user-emacs-directory)
  "Path to the user's personal lisp directory.")

(defvar my/org-person-dir "~/Dropbox/Docs/Person/"
  "Directory for personal org files.")

(setq org-directory (expand-file-name "~/Dropbox/Docs/org"))


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
        (tex-bin "/Library/TeX/texbin")
	(emacs-bin "/Applications/Emacs.app/Contents/MacOS/bin"))
    (dolist (path (list brew-bin tex-bin emacs-bin))
      (when (file-directory-p path)
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat path ":" (getenv "PATH")))))
    
    ;; Dired ls 설정
    (let ((gls-prog (executable-find "gls")))
      (setq insert-directory-program (or gls-prog "ls")
            dired-use-ls-dired (if gls-prog t nil)))))


;; =======================================
;;; Native Compilation Settings 
;; =======================================
(when (and my-macOS-p (fboundp 'native-comp-available-p) (native-comp-available-p))
  (let* ((gcc-bin "/opt/homebrew/bin/gcc-15")
         (gcc-lib-1 "/opt/homebrew/lib/gcc/15")
         (gcc-lib-2 "/opt/homebrew/Cellar/gcc/15.2.0/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
         (sdk-lib "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))

    ;; 1. 컴파일러 실행 파일 경로 설정
    (setq native-comp-driver-path gcc-bin)

    ;; 2. LIBRARY_PATH 통합 설정
    ;; gcc 내부 라이브러리(emutls_w)와 시스템 SDK(System) 경로를 모두 포함합니다.
    (setenv "LIBRARY_PATH" 
            (concat (getenv "LIBRARY_PATH") ":" 
                    gcc-lib-1 ":" 
                    gcc-lib-2 ":" 
                    sdk-lib))

    ;; 3. 비동기 컴파일 경고/에러 팝업 억제
    (setq native-comp-async-report-warnings-errors 'silent)))


;; =======================================
;;; Load custom packages
;; =======================================
(add-to-list 'load-path my/lisp-path)

(require 'my-completion)
(require 'my-dired-custom)
;; (require 'my-window)
(require 'my-org-custom)
(require 'my-useful-custom)
(require 'my-search)
(require 'my-todays-pop)
(require 'my-radio-direct)
(require 'my-viewmode-custom)
(require 'my-keys)


;; =======================================
;;; MacOS keyboard
;; =======================================
(when my-macOS-p
  (setq mac-pass-command-to-system nil)
  ;; [왼쪽] Opt(Super) / Cmd(Meta)
  (setq ns-option-modifier 'super)
  (setq ns-command-modifier 'meta))
  ;; [오른쪽] Cmd(Meta) / Opt(Control)
  ;; (setq ns-right-command-modifier 'meta)  ;'meta
  ;; (setq ns-right-option-modifier 'control)) ;'control


;; =======================================
;;; Emacs UI and behavior
;; =======================================
(use-package emacs
  :init
  (setq default-directory (expand-file-name "~/Dropbox/Docs/org")
        temporary-file-directory (expand-file-name "tmp/" user-emacs-directory))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . my-open-pdf-with-external-app))
  
  :hook ((text-mode . visual-line-mode)
	 (focus-in . my/deactivate-input-method))
  
  :custom
  ;; UI 및 상태 정보 관련
  (use-short-answers t)               ; y/n으로 대답 단축
  (column-number-mode t)              ; 열 번호 표시
  (display-time-mode t)               ; 시간 표시
  ;;시각 효과, 가독성
  (line-spacing 0.2)                  ; 줄 간격 여백
  (text-scale-mode-step 1.02)         ; 텍스트 크기 조절 단계
  (frame-resize-pixelwise t)          ; pixcel 단위
  ;; 스크롤, 탐색
  (scroll-margin 3)                   ; 상하 2줄 여백 유지하며 스크롤
  (scroll-conservatively 101)         ; 화면 점프 없이 부드럽게 한 줄씩 스크롤
  (scroll-error-top-bottom t)         ; 문서 끝에서 방향키 입력 시 커서 이동 후 경고
  ;;마크, 히스토리 
  (set-mark-command-repeat-pop t)     ; C-u C-SPC 이후 C-SPC만으로 계속 점프
  (mark-ring-max 16)                  ; 버퍼 내 마크 저장 개수
  (global-mark-ring-max 32)           ; 전체 버퍼 마크 저장 개수
  ;; 편집 행동 관련
  (kill-whole-line 1)                 ; 줄 전체 삭제 시 줄바꿈까지 삭제
  (next-line-add-newlines nil)        ; 문서 끝에서 C-n 눌러도 새 줄 추가 안 함
  (enable-recursive-minibuffers t)    ; 미니버퍼 내에서 다른 미니버퍼 호출 허용
  (create-lockfiles nil)
  :config
  (minibuffer-depth-indicate-mode 1)  ; 미니버퍼 재귀 깊이
  :bind
  (("C-x z"     . nil)
   ("C-x m"     . nil)
   ("C-x f"     . toggle-frame-fullscreen)
   ("M-;"       . comment-line)
   ("M-s u"     . my-search-unified)
   ("C-a"       . my-smart-beginning-of-line)
   ("C-g"       . my-keyboard-quit-dwim)
   ("<escape>"  . my-keyboard-quit-dwim)))
   ;; ("C-c E". my-window-popup-eshell)


(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)      ; 24-hour system
  (display-time-format "%Y-%m-%d (%a) %H:%M")
  (display-time-day-and-date t)
  (display-time-load-average nil))  ; mode-line-misc-info average nil


;; =======================================
;;; Auto-revert (Dropbox Sync Optimization)
;; =======================================
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 60)         ; 60초 간격
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :hook (focus-in . (lambda ()
                      (when (fboundp 'auto-revert-buffers)
                        (auto-revert-buffers))))
  :config
  (global-auto-revert-mode t))


;; ========================================================
;; Window Management (Macbook Air 13")
;; ========================================================
(use-package emacs
  :if my-macOS-p
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
;;; Bookmark
;; =======================================
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))


;; =======================================
;;; Register
;; =======================================
(use-package register
  :ensure nil
  :config
  (let ((org-dir my/org-person-dir)
	(conf-dir user-emacs-directory))
    (set-register ?i `(file . ,(expand-file-name "init.el" conf-dir)))
    (set-register ?l `(file . ,(expand-file-name "lisp/" conf-dir)))
    (set-register ?r `(file . ,(concat org-dir "cReading.org")))
    (set-register ?d `(file . ,(concat org-dir "Daily.org")))
    (set-register ?n `(file . ,(concat org-dir "cNotes.org")))
    (set-register ?p `(file . ,(expand-file-name "~/Dropbox/Docs/pdf"))))
  (set-register ?o `(file . ,default-directory))
  :custom
  (register-preview-delay 0.5))


;; =======================================
;;; Locale and Korean settings
;; =======================================
(use-package emacs
  :init
  (setenv "LANG" "ko_KR.UTF-8")
  (setenv "LC_COLLATE" "C")
  (set-locale-environment "ko_KR.UTF-8")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  :custom
  (default-input-method "korean-hangul")
  (input-method-verbose-flag nil)
  (input-method-highlight-flag nil))


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
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>"   . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-symbiosis))


;; =======================================
;;; Session and Place Persistence
;; =======================================
(use-package savehist
  :ensure nil
  :demand t
  :init (savehist-mode 1)
  :custom
  (history-length 10))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))


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
  (("C-x <left>"  . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x <up>"    . windmove-up)
   ("C-x <down>"  . windmove-down)))


;; =======================================
;;; winner
;; =======================================
(use-package winner
  :ensure nil    ;built-in
  :init
  (winner-mode 1))


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
;;; Modeline
;; =======================================
(defvar my/indicator-image-dir 
  (expand-file-name "img-indicator/" user-emacs-directory))
(defvar ko-indicator 
  (create-image (expand-file-name "han2.tiff" my/indicator-image-dir) 
                'tiff nil :ascent 'center))
(defvar en-indicator 
  (create-image (expand-file-name "qwerty.tiff" my/indicator-image-dir) 
                'tiff nil :ascent 'center))
(defvar mode-line-use-images-p 
  (and (display-graphic-p) (image-type-available-p 'tiff)))
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
                (:eval
                 (let* ((is-ko (equal current-input-method "korean-hangul"))
                        (label (if is-ko "KO" "EN")))
                   (propertize label
                               'display (when mode-line-use-images-p 
                                          (if is-ko ko-indicator en-indicator))
                               'help-echo label)))
                "   "
                "Ⓗ "
                mode-line-buffer-identification
                mode-line-frame-identification
                "  "
                ;; mode-line-modes
                mode-line-format-right-align
                mode-line-position
                " Ⓨ "
                mode-line-misc-info))


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
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  ;; Magit이 전체 화면을 차지하지 않고, 현재 창 구성을 최대한 유지
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; =======================================
;;; expand-region
;; =======================================
(use-package expand-region
  :ensure nil
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))


;; =======================================
;;; eldoc
;; =======================================
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . eldoc-mode))


;; =======================================
;;; Helpful
;; =======================================
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)   ; 함수, 매크로 등 호출 가능한 모든 것
   ("C-h v" . helpful-variable)   ; 변수 설정 확인 시 유용
   ("C-h k" . helpful-key)        ; 특정 키가 어떤 기능을 하는지 확인
   ("C-h x" . helpful-command)    ; M-x 명령 확인
   ("C-c C-d" . helpful-at-point) ; 현재 커서 아래의 심볼 바로 확인
   ("C-h F" . helpful-function))  ; 호출 가능 여부와 상관없이 '함수'만 확인
  :custom
  (helpful-max-lines 50))


;; =======================================
;;; Manual Session Management
;; =======================================
(use-package desktop
  :ensure nil
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-save 'if-exists)
  (desktop-buffers-not-to-save "\\(^\\*\\|\\.log$\\)")
  (desktop-save-mode nil) 
  :config
  (defun my/desktop-save-at-point ()
    "Save all current buffers and window configurations."
    (interactive)
    (desktop-save user-emacs-directory)
    (message "✅ [Layout Saved] Current configuration has been recorded."))

  (defun my/desktop-read-at-point ()
    "Restore the saved desktop session."
    (interactive)
    (desktop-read user-emacs-directory)
    (message "✅ [Layout Restored] Previous session has been restored."))
  :bind
  (("C-x r S" . my/desktop-save-at-point)  ; Save Layout
   ("C-x r R" . my/desktop-read-at-point))) ; Restore Layout

