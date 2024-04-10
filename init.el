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
(require 'package)
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
;;; 외양
;; ======================================
(tool-bar-mode -1)  ; 도구상자 비활성
(toggle-scroll-bar -1)
(setq inhibit-startup-message t)     ;시작 메시지  안나오게
(setq visible-bell t )                             ; 경로 벨 대신 시각적인 벨로 표시
(setq initial-scratch-message nil)
(setq use-dialog-box nil)
(setq-default line-spacing 0.2)        ; 줄 간격 1.5
;; (setq frame-title-format "| dole's Emacs | %b |")

;; ======================================
;;; 작은 설정 들
;; ======================================
;; defult-directory
;; (setq default-directory (if my-mactop-p "~/Dropbox/Docs/org/" "~/Docs/org/")
;;       temporary-file-directory (if my-mactop-p "~/Dropbox/Docs/tmpdir/" "~/Docs/tmpdir/"))
(setq default-directory  "~/Docs/org/")
(setq temporary-file-directory "~/Docs/tmpdir/")
(setq make-backup-files nil)
(setq kill-whole-line 1)
(setq search-highlight t)
;;      display-time-format "%b-%d(%a) %H:%M")

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
	modus-themes-mixed-fonts t
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
(require 'my-org-custom)               ;org-mode
(require 'my-org-latex-custom)     ;org export pdf
(require 'my-dired-custom)            ;dired
(require 'my-reading-mode-custom)   ;reading mode
(require 'my-play-streaming)        ;radio 청취
(require 'my-emacs-super-keys)  ; minor. super key

;; ======================================
;;; Keyboard for MacOS
;; ======================================
;; (when (equal system-type 'darwin)
;; ;;(when (string= system-name "MacBookAir.local")
;;   (setq mac-option-modifier 'super)
;;   (setq mac-command-modifier 'meta))

;; ======================================
;;; 단축키 prefix key
;; ======================================
(global-unset-key [f11])  ;remove toggle-frame-fullscreen/MacOS
(global-unset-key (kbd "C-x o"))  ;remove  'other-window
(global-unset-key (kbd "s-t"))      ;remove set font
(global-unset-key (kbd "s-c"))     ;remove Copy
(global-unset-key (kbd "s-f"))     ;remove I-search forward
(global-unset-key (kbd "s-g"))    ;  
;; (global-unset-key (kbd "s-v"))     ;remove Paste. alfred paste 
;; (global-set-key (kbd "C-x C-m") 'execute-extended-command) ; M-x
(global-set-key (kbd "M-o") 'other-window)
;; ======================================
(defvar-keymap my-prefix-map
  :doc "my prefix map."
  "p" 'my-popmark
;;  "g" 'consult-grep
  "e" 'eshell
;;  "k" 'keycast-mode-line-mode
  "l" 'my-org-latex-custom
  "m" 'modus-themes-toggle
;;  "f" 'toggle-frame-fullscreen
  "r" 'toggle-my-reading-mode
  ;;  "s" 'toggle-streaming)
  )

(keymap-set global-map "s-m" my-prefix-map)
;; --------------------------------------------------------
;; base-dir 변수 사용하여 Mac 여부에 따라 기본 디렉토리 선택
;; 중복 코드 회피, my-open-directory 및 my-open-file 함수 사용
(defun my-popmark (choice)
  "Choices for faverite directories and files."
  (interactive "c\[O]org(Dir) | [E]macs(Dir) | [P]pdf(Dir) | [i]nit | [t]asks | [c]Notes | [d]aily | [f]arm")
  (let ((base-dir "~/"))
;;    (let ((base-dir (if my-mactop-p "~/Dropbox/" "~/")))
    (cond
     ((eq choice ?E) (my-open-directory "emacs"))
     ((eq choice ?O) (my-open-directory "Docs/org"))
     ((eq choice ?P) (my-open-directory "Docs/pdf"))
     ((eq choice ?i) (my-open-file "emacs/init.el"))
     ((eq choice ?t) (my-open-file "Docs/org/Tasks.org"))
     ((eq choice ?c) (my-open-file "Docs/org/cNotes.org"))
     ((eq choice ?d) (my-open-file "Docs/org/Daily.org"))
     ((eq choice ?f) (my-open-file "Docs/org/dFarmNote.org"))
     (t (message "Quit")))))

(defun my-open-directory (dir)
  "Open a directory based on the platform and given subdirectory."
  (dired (concat base-dir dir)))

(defun my-open-file (file)
  "Open a file based on the platform and given file path."
  (find-file (concat base-dir file))
  (message "Opened: %s" (buffer-name)))
;;
;; ======================================
;;; 로케일, 한글
;; ======================================
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")		  ;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	  ;kbd 한글 S-SPC

;; ======================================
;;; 글꼴 fonts
;; ======================================
(when (display-graphic-p)
;;(set-frame-font "Noto Sans Mono CJK KR")
(set-face-attribute 'default nil
		    :family "Noto Sans CJK KR"    ;Hack, Menlo;Noto Sans CJK KR
		    :height 160)
(set-face-attribute 'fixed-pitch nil
		    :family "Noto Sans Mono CJK KR")
(set-face-attribute 'variable-pitch nil :family "Noto Sans CJK KR")
;;(when (display-graphic-p)
(set-fontset-font nil 'hangul (font-spec :family "Noto Sans CJK KR")))

;; ======================================
;;; korean calendar
;; ======================================
(use-package calendar
  :config  
  (setq calendar-week-start-day 0
	calendar-day-name-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
;;	calendar-day-header-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"]))
;;
;;; calendar layout 보정. D2coding size
(defun cal-fixLayout ()
  (face-remap-add-relative 'default '(:family "Noto Sans Mono CJK KR" :height 150)))           
(add-hook 'calendar-mode-hook 'cal-fixLayout)

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
  (recentf-max-saved-items 50))

;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-setup-side-window-right))

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
;; annotations in the minibuffer
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
;;   ("C-c r d" . consult-dir)
   ("C-c g" . consult-grep)
   ("C-c r o" . consult-outline)
   ("C-c r t" . consult-theme))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; ======================================
;;; consult-dir
;; ======================================
;; insert paths into minibuffer prompts in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-c d" . consult-dir)
         :map vertico-map
         ("C-c d" . consult-dir)))

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
;;; electric-pair-mode
;; ======================================-
(use-package electric
  :ensure nil  ;built in
  :config
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\[ . ?\])
                              (?\( . ?\))))
  (electric-pair-mode t))
(add-hook 'org-mode-hook (lambda ()   ; pair-mode '< '제외
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; =======================================
;;; corfu
;; ======================================-
(setq completion-cycle-threshold 3
      tab-always-indent 'complete)
(use-package corfu
  :ensure t
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ("S-<return>" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

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

(use-package nerd-icons-completion
  :after completion
  :config
  (add-hook 'completion-list-mode-hook 'nerd-icons-completion-mode))

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
(defun search-web (engine query)
  "검색 엔진, 검색"
  (interactive
   (list
    (completing-read "검색 선택 (google/naver): " '("google" "naver"))
    (read-string "검색어 입력: ")))
  (let ((url (cond
               ((string= engine "google") (concat "https://www.google.com/search?q=" (url-hexify-string query)))
               ((string= engine "naver") (concat "https://search.naver.com/search.naver?query=" (url-hexify-string query)))
               (t (error "검색 엔진 err!")))))
    (browse-url url)))

(defun my/today-custom-format (fm)
  "Inserts today() at point in the specified format."
  (interactive
   (list (completing-read "Select: " '("dash" "dot"))))
  (let ((format-string
         (cond
          ((string= fm "dash") "%Y-%m-%d")
          ((string= fm "dot") "%Y.%m.%d")
          (t (error "Invalid date format specified")))))
    (insert (format-time-string format-string))))

(defun my/region-colorful (color)
  "선택 텍스트, 색상 COLOR 지정. 저장불가"
  (interactive
   (list (completing-read "Select: "
                          '("red" "blue" "green" "yellow" "orange"))))
  (put-text-property (region-beginning) (region-end) 'font-lock-face `((foreground-color . ,color))))

