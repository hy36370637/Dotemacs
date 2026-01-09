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

(require 'my-completion)
(require 'my-dired-custom)
(require 'my-org-custom)
(require 'my-useful-custom)
(require 'my-search)
(require 'my-todays-pop)
(require 'my-radio-direct)

;; =======================================
;;; MacOS keyboard
;; =======================================
(when my-macOS-p
  (setq mac-pass-command-to-system nil)
  ;; [왼쪽] Opt(Super) / Cmd(Meta)
  (setq ns-option-modifier 'super)
  (setq ns-command-modifier 'meta)
  ;; [오른쪽] Cmd(Meta) / Opt(Control)
  (setq ns-right-command-modifier 'meta)
  (setq ns-right-option-modifier 'control))

;; =======================================
;;; macOS Input Source Control (im-select)
;; =======================================
;; im-select 설치를 위한 탭 추가 및 설치
;; brew tap daipeihust/tap
;; brew install im-select
(defvar my/im-select-path nil
  "im-select 실행 파일 경로 캐시")

(defun my/mac-switch-to-english ()
  "Switches macOS input method to English when Emacs gains focus."
  (when (and my-macOS-p (display-graphic-p))
    (unless my/im-select-path
      (setq my/im-select-path (executable-find "im-select")))
    (when my/im-select-path
      (call-process my/im-select-path nil 0 nil "com.apple.keylayout.ABC"))))

(add-hook 'focus-in-hook #'my/mac-switch-to-english)

;; =======================================
;;; Emacs UI and behavior
;; =======================================
(use-package emacs
  :init
  (setq default-directory (expand-file-name "~/Dropbox/Docs/org")
        temporary-file-directory (expand-file-name "tmp/" user-emacs-directory))
  :hook (text-mode . visual-line-mode)
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
  (global-auto-revert-mode t)         ; 외부에서 변경된 파일 자동 새로고침
  (next-line-add-newlines nil)        ; 문서 끝에서 C-n 눌러도 새 줄 추가 안 함
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)  ; 미니버퍼 재귀 깊이
  :bind
  (("C-x f" . nil)
   ("C-x m" . nil)
   ("C-x z" . nil)
   ("C-c 9" . toggle-frame-maximized)
   ("C-c 0" . toggle-frame-fullscreen)))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)                      ; 24-hour system
  (display-time-format "%Y-%m-%d (%a) %H:%M")
  (display-time-day-and-date t)
  (display-time-load-average nil))                  ; mode-line-misc-info average nil

;; ========================================================
;; Window Management (Macbook Air 13")
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
;;; Bookmark
;; =======================================
(use-package bookmark
  :ensure nil                               ; built in
  :custom
  (bookmark-save-flag 1)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))

;; =======================================
;;; Register
;; =======================================
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0)               ; 지연 없이 바로 프레뷰 표시
  :config
  (let ((org-dir my/org-person-dir)
	(conf-dir user-emacs-directory))
    (set-register ?i `(file . ,(expand-file-name "init.el" conf-dir)))
    (set-register ?l `(file . ,(expand-file-name "lisp/" conf-dir)))
    (set-register ?r `(file . ,(concat org-dir "cReading.org")))
    (set-register ?d `(file . ,(concat org-dir "Daily.org")))
    (set-register ?n `(file . ,(concat org-dir "cNotes.org"))))
  (set-register ?o `(file . ,default-directory)))

;; =======================================
;;; Locale and Korean settings
;; =======================================
(use-package emacs
  :init
  (setenv "LANG" "ko_KR.UTF-8")
  (setenv "LC_COLLATE" "C")
  (set-locale-environment "ko_KR.UTF-8")
  :custom
  (input-method-verbose-flag nil)
  (input-method-highlight-flag nil))

;; =======================================
;;; Fonts
;; =======================================  
(use-package emacs
  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil :family "Noto Sans KR" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))
  (add-hook 'prog-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Noto Sans Mono CJK KR"))))

;; =======================================
;;; Theme
;; =======================================
(use-package emacs
  :bind 
  ("<f5>" . modus-themes-toggle)
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mode-line '(accented borderless padded)
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold))
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  (load-theme 'modus-operandi-tinted t))
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
;;; Session and Place Persistence
;; =======================================
(use-package savehist
  :ensure nil
  :demand t
  :custom
  (history-length 10)
  :init (savehist-mode 1))

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
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after (marginalia nerd-icons)
  :config  (nerd-icons-completion-mode 1))

;; =======================================
;;; windmove
;; =======================================
(use-package windmove
  :ensure nil   ;built-in
  :bind
  (("<s-left>" . windmove-left)
   ("<s-right>" . windmove-right)
   ("<s-up>" . windmove-up)
   ("<s-down>" . windmove-down)))

;; =======================================
;;; windmove
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
  :custom (which-key-idle-delay 0.2))

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
                mode-line-modes
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
;;; Key-binding
;; =======================================
(defvar-keymap my-emacs-prefix-map
  :doc "my-emacs-prefix-keymap"
  :name "My Personal Map"
  "b" #'eval-buffer
  "c" #'my-capture-cReading-access
  "i" my-image-prefix-map
  "m" my-radio-prefix-map
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "w" #'my-pair-pairs-wrap)

(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "c" "Reading"
  "i" "Image"
  "m" "Radio"
  "s" "Search"
  "t" "Todays-pop"
  "w" "Pairs-wrap")

;; 4. 전역 키 바인딩
(keymap-set global-map "C-c j" my-emacs-prefix-map)
