;; ======================================
;;; Speed up emacs
;; ======================================
;; 가비지 수집 호출 횟수 줄이기
(setq gc-cons-threshold 100000000)
(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

;; ======================================
;;; custom file
;; ======================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (when (file-exists-p custom-file) (load custom-file 't))

;; ======================================
;;; package source list
;;; ======================================
;; (require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))
;;	("org" . "https://orgmode.org/elpa/")
;;	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; ======================================
;;; use-package
;; ======================================
(require 'use-package)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; ;; (setq use-package-always-ensure nil)

;; ======================================
;;; system-info
;; ======================================
(defvar my-laptop-p (eq system-type 'gnu/linux))
(defvar my-mactop-p (eq system-type 'darwin))
;; (defvar my-Macbook-p (string-equal system-name "MacBookAir.local"))

;; ======================================
;;; MacOS keyboard
;; ======================================
;; (setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'meta)
;; (setq ns-right-command-modifier 'super)
;; (setq mac-right-option-modifier 'control)

;; ======================================
;;; 외양
;; ======================================
(tool-bar-mode -1)                            ; 도구상자 비활성
;; (menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-message t)     ;시작 메시지  안나오게
(setq visible-bell t )                             ; 경로 벨 대신 시각적인 벨로 표시
(setq initial-scratch-message nil)
(setq use-dialog-box nil)
(setq-default line-spacing 0.2)        ; 줄 간격 1.5
(setq-default cursor-type 'bar)	       ;cursor type hbar,bar,box
;; (setq frame-title-format "| dole's Emacs | %b |")

;; ======================================
;;; 작은 설정 들
;; ======================================
(setq default-directory  "~/Docs/org/")
(setq temporary-file-directory "~/Docs/tmpdir/")
(setq make-backup-files nil)
(setq kill-whole-line 1)
(setq search-highlight t)
;;      display-time-format "%b-%d(%a) %H:%M")
;;; Scroll setting
(setq scroll-margin 7)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

;; ======================================
;;; 자잘한 필수 모드
;; ======================================
(save-place-mode 1)
(global-font-lock-mode 1)
(global-visual-line-mode t) ;word wrap
(global-auto-revert-mode 1)
(transient-mark-mode t)
(column-number-mode t)
(display-time-mode 1)

;; ======================================
;;; modus theme
;; ======================================
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-mixed-fonts nil	;default nil
	modus-themes-variable-pitch-ui nil
	modus-themes-custom-auto-reload t
	modus-themes-disable-other-themes t))
;; Remove the border
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

;; ======================================
;;; start emacs (load theme by time)
;; ======================================
;; display-time-mode 사용.
(defun set-theme-by-time ()
  "set theme by time, loading emacs"
  (let ((current-hour (string-to-number (substring (current-time-string) 11 13))))
    (if (and (>= current-hour 9) (< current-hour 17))  ; 9시부터 17시까지
        (load-theme 'modus-operandi)
      (load-theme 'modus-vivendi))))
(set-theme-by-time)

;; ======================================
;;; exec-path-from-shell
;; ======================================
;; MacOS PATH 설정
(use-package exec-path-from-shell
  :ensure t
  :if my-mactop-p
  :init
  (exec-path-from-shell-initialize))

;; ======================================
;;; load-my-package
;; ======================================
;; (if my-mactop-p
;;     (add-to-list 'load-path "~/Dropbox/emacs/lisp/")
;;   (add-to-list 'load-path "~/emacs/lisp/"))
(add-to-list 'load-path "~/emacs/lisp/")
(require 'my-org-custom)                       ; org-mode
(require 'my-org-latex-custom)            ; org export pdf
(require 'my-dired-custom)                    ; dired
(require 'my-reading-mode-custom)   ; reading mode
(require 'my-play-streaming)                 ; radio 청취
(require 'my-emacs-super-keys)            ; minor. super key
(require 'my-completion)                       ;completion
(require 'my-web-search)

;; ======================================
;;; Keyboard for MacOS
;; ======================================
;; (when my-mactop-p
;;     (setq mac-option-modifier 'super)
;;     (setq mac-command-modifier 'meta)
;;   )

;; ======================================
;;; 단축키 prefix key
;; ======================================
(global-unset-key [f11])  ; able expose
(global-unset-key (kbd "C-x o"))  ;remove  'other-window
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; M-x
(global-set-key (kbd "M-o") 'other-window)
;;; 
(defvar-keymap my-prefix-map
  :doc "my prefix map."
  "c" 'select-special-character
  "d" 'my/insert-today
  "h" 'my-hunspell-check
  "k" 'keycast-mode-line-mode
  "m" 'modus-themes-toggle
  "r" 'toggle-my-reading-mode
  "s" 'my/region-search-web
  "t" 'set-transparency
  "w" 'naver-weather-search)

(keymap-set global-map "s-t" my-prefix-map)
;; --------------------------------------------------------
;; (defun my-popmark (choice)
;; ;; base-dir 변수 사용하여 Mac 여부에 따라 기본 디렉토리 선택
;; ;; 중복 코드 회피, my-open-directory 및 my-open-file 함수 사용
;;   "Choices for faverite directories and files."
;;   (interactive "c\ [P]pdf(Dir) | [i]nit | [t]asks | [c]Notes | [d]aily | [f]arm")
;;   (let ((base-dir "~/"))
;; ;;    (let ((base-dir (if my-mactop-p "~/Dropbox/" "~/")))
;;     (cond
;;      ((eq choice ?P) (my-open-directory "Docs/pdf"))
;;      ((eq choice ?i) (my-open-file "emacs/init.el"))
;;      ((eq choice ?t) (my-open-file "Docs/org/Tasks.org"))
;;      ((eq choice ?c) (my-open-file "Docs/org/cNotes.org"))
;;      ((eq choice ?d) (my-open-file "Docs/org/Daily.org"))
;;      ((eq choice ?f) (my-open-file "Docs/org/dFarmNote.org"))
;;      (t (message "Quit")))))

;; (defun my-open-directory (dir)
;;   "Open a directory based on the platform and given subdirectory."
;;   (dired (concat base-dir dir)))

;; (defun my-open-file (file)
;;   "Open a file based on the platform and given file path."
;;   (find-file (concat base-dir file))
;;   (message "Opened: %s" (buffer-name)))

;; ======================================
;;; 로케일, 한글
;; ======================================
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")	                	  ;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	  ;kbd 한글 S-SPC
(setq input-method-verbose-flag nil)
(setq input-method-highlight-flag nil)          ;입력 글자 밑줄방지

;; ======================================
;;; 글꼴 fonts
;; ======================================
(when (display-graphic-p)
;;(set-frame-font "Noto Sans Mono CJK KR")
  (set-face-attribute 'default nil
		      :family "Noto Sans CJK KR"    ;Hack, Menlo;Noto Sans CJK KR
		      :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans CJK KR")
  (set-fontset-font nil 'hangul (font-spec :family "Noto Sans CJK KR")))

;; ======================================
;;; korean calendar
;; ======================================
(use-package calendar
  :config  
  (setq calendar-week-start-day 0
	calendar-day-name-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
;;	calendar-day-header-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"])

;;; calendar layout 보정. D2coding size
(defun cal-fixLayout ()
  (face-remap-add-relative 'default '(:family "Noto Sans Mono CJK KR" :height 160)))           
(add-hook 'calendar-mode-hook 'cal-fixLayout)

;; ======================================
;;; holidays
;; ======================================
(use-package holidays
  :config
  (setq my-holidays			;공휴일+생일
	'((holiday-fixed 1 1 "새해")
	  (holiday-chinese  1  1 "설날")
	  (holiday-fixed 1 10 "딸 생일")
	  (holiday-fixed 3 1 "삼일절")
	  (holiday-fixed 3 19 "결혼일")
	  (holiday-chinese  4  8 "석탄일")
	  (holiday-fixed 5 5 "어린이날")
	  (holiday-fixed 6 6 "현충일")
	  (holiday-fixed 6 10 "아들생일")
	  (holiday-fixed 7 17 "제헌절")
	  (holiday-fixed 8 15 "광복절")
	  (holiday-chinese  8 15 "추석")
	  (holiday-fixed 10 3 "개천절")
	  (holiday-fixed 10 9 "한글날")
	  (holiday-fixed 12 25 "성탄절")))
  
(setq 24solar-holidays			;24절기
	'((holiday-fixed 2 4 "입춘(새봄)")
	  (holiday-fixed 2 19 "우수(눈녹음)")
	  (holiday-fixed 3 5 "경칩(겨울잠 깸)")
	  (holiday-fixed 3 20 "춘분(낮 길어짐)")
	  (holiday-fixed 4 5 "청명")
	  (holiday-fixed 4 20 "곡우(봄비)")
	  (holiday-fixed 5 5 "입하")
	  (holiday-fixed 5 21 "소만(볕 잘듬)")
	  (holiday-fixed 6 6 "망종(씨앗)")
	  (holiday-fixed 6 21 "하지")
	  (holiday-fixed 7 7 "소서(더위 시작)")
	  (holiday-fixed 7 22 "대서(가장 더움)")
	  (holiday-fixed 8 7 "입추")
	  (holiday-fixed 8 23 "처서(가을바람)")
	  (holiday-fixed 9 7 "백로(이슬)")
	  (holiday-fixed 9 22 "추분(밤 길이)")
	  (holiday-fixed 10 8 "한로(이슬)")
	  (holiday-fixed 10 23 "상강(서리)")
	  (holiday-fixed 11 7 "입동")
	  (holiday-fixed 11 22 "소설(눈 시작)")
	  (holiday-fixed 12 7 "대설(눈 많음)")
	  (holiday-fixed 12 22 "동지")
	  (holiday-fixed 1 5 "소한")
	  (holiday-fixed 1 20 "대한")))
;; 기본 휴일 설정 초기화
(setq holiday-general-holidays nil)
(setq holiday-local-holidays nil)
(setq holiday-other-holidays nil)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-oriental-holidays nil)
(setq calendar-mark-holidays-flag t))	;holiday display
(setq calendar-holidays (append my-holidays 24solar-holidays)))

(custom-set-faces
  '(holiday ((t (:foreground "red" :weight bold)))))

;; ======================================
;;; helpful
;; ======================================
(use-package helpful
  :ensure t
  :bind(("C-h k" . helpful-key)
	("C-c C-d" . helpful-at-point)
	("C-h C" . helpful-command)
	("C-h o" . helpful-symbol)))

;; ======================================
;;; recentF
;; ======================================
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 30))

;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure nil
  :init
  (which-key-mode)
  :config
  ;; (which-key-setup-side-window-right)
  (setq which-key-idle-delay 0.2))

;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))

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
;; advenced completion stlye
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ======================================
;;; consult
;; ======================================
;; enhanced minibuffer commands, search
(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("C-c r b" . consult-bookmark)
   ("C-c r o" . consult-outline)
   ("C-c r t" . consult-theme))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; ======================================
;;; consult-dir
;; ======================================
;; insert paths into minibuffer prompts in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-c r d" . consult-dir)
         :map vertico-map
         ("C-c r d" . consult-dir)))

;; ======================================
;;; embark
;; ======================================
;; extended minibuffer actions and context menu
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ======================================
;;; gnus
;; ======================================
;; (use-package gnus
;;   :ensure nil
;;   :if my-Macbook-p
;;   :config
;;   (setq user-mail-address "under9@icloud.com"
;; 	user-full-name "Ho-Young")
;;   (setq gnus-select-method
;; 	'(nnimap "icloud"
;; 		 (nnimap-address "imap.mail.me.com")
;; 		 (nnimap-server-port 993)
;; 		 (nnimap-stream ssl)
;; 		 (nnir-search-engine imap)
;; 		 (nnmail-expiry-target "nnimap+icloud:Deleted Messages")
;; 		 (nnimap-authinfo-file "~/.authinfo")))
;;   (setq message-send-mail-function 'smtpmail-send-it
;; 	smtpmail-default-smtp-server "smtp.mail.me.com"
;; 	smtpmail-smtp-server "smtp.mail.me.com"
;; 	smtpmail-smtp-service 587
;; 	smtpmail-stream-type 'starttls
;; 	smtpmail-smtp-user "under9@icloud.com"
;; 	smtpmail-debug-info t
;; 	gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

;; =======================================
;;; all-the-icons
;; ======================================-
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :after dired
  :if (display-graphic-p)
  :config
(add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

;; (use-package nerd-icons-completion
;;   :after completion
;;   :if (display-graphic-p)
;;   :config
;;   (add-hook 'completion-list-mode-hook 'nerd-icons-completion-mode))

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
;; 단순 버젼 original
;; (setq-default mode-line-format nil)
(setq-default mode-line-format
	      '("%e "
		mode-line-front-space
		(:eval (if (string= current-input-method "korean-hangul")
				       "KO"
				     "EN"))
		" Ⓗ "
		mode-line-buffer-identification       
		mode-line-frame-identification
		" Ⓨ "
		mode-line-modes
		mode-line-format-right-align  ;; emacs 30
		mode-line-position
		"Ⓚ "
		mode-line-misc-info))

;; ======================================
;;; keycast
;; ======================================
(use-package keycast
  :ensure t
  :config
  (setq keycast-mode-line-insert-after 'mode-line-modes)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-mode -1))

;; ======================================
;;; rainbow-delimiters
;; ======================================
;; 괄호, 중괄호, 각종 쌍을 시각적(무지개색) 구분
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ======================================
;;; pdf-view, tools
;; ======================================
;; only linux
(use-package pdf-tools
  :ensure nil
  :if my-laptop-p 
  :mode ("\\.pdf\\'" . pdf-view-mode) ; Automatically open PDFs in pdf-view-mode
  :config
  (setq pdf-view-display-size 'fit-width) ; Set the default zoom level
  (pdf-tools-install))

;; ======================================
;;; etc my-custom-fuction
;; ======================================
(defun my/insert-today (fm)
  "Inserts today() at point in the specified format."
  (interactive
   (list (completing-read "Select: " '("dash" "dot"))))
  (let ((format-string
         (cond
          ((string= fm "dash") "%Y-%m-%d")
          ((string= fm "dot") "%Y.%m.%d")
          (t (error "Invalid date format specified")))))
    (insert (format-time-string format-string))))

;; ;; 배경 투명 toggle
(defun set-transparency (&optional alpha-level)
  "Set the transparency of the Emacs frame."
  (interactive "P")
  (setq alpha-level (if alpha-level
                        (prefix-numeric-value alpha-level)
                      75)) ;; 기본값 설정
  (set-frame-parameter (selected-frame) 'alpha (cons alpha-level alpha-level)))

(defun toggle-transparency ()
  "Toggle transparency of the Emacs frame."
  (interactive)
  (let ((current-alpha (frame-parameter nil 'alpha)))
    (if (or (equal current-alpha '(0 . 0)) (equal current-alpha '(100 . 100)))
        (set-transparency 75)
      (set-transparency 100))))
