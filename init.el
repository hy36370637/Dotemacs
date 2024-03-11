;; ======================================
;;; Speed up emacs
;; --------------------------------------
;; 가비지 수집 호출 횟수 줄이기
(setq gc-cons-threshold 10000000)
(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))
;;
;; ======================================
;;; custom file
;; --------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (when (file-exists-p custom-file) (load custom-file 't))
;;
;; ======================================
;;; package source list
;; --------------------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))
;;	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
;; 
;; ======================================
;;; use-package
;; --------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (setq use-package-always-ensure nil)
;;
;; ======================================
;;; system-info
;; --------------------------------------
(defvar my-laptop-p (eq system-type 'gnu/linux))
(defvar my-mactop-p (eq system-type 'darwin))
;;
;; ======================================
;;; 외양
;; --------------------------------------
;;; hidden menu Bar
(menu-bar-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;;; hidden start message
(setq inhibit-startup-message t
      visible-bell t
      initial-scratch-message nil
      use-dialog-box nil)
(setq-default line-spacing 0.2)    ; 줄 간격 1.5
;; (setq frame-title-format "| dole's Emacs | %b |")
;;
;; ======================================
;;; 작은 설정 들
;; --------------------------------------
;; (setq global-auto-revert-non-file-buffers t)
(if my-mactop-p               ;(eq system-type 'darwin)
    (setq default-directory "~/Dropbox/eDoc/org/"
	  temporary-file-directory "~/Dropbox/eDoc/tmpdir/")
  (setq default-directory "~/eDoc/org/"
	temporary-file-directory "~/eDoc/tmpdir/"))
;;
(setq make-backup-files nil
      kill-whole-line 1
      search-highlight t)
;;; for corfu
(setq completion-cycle-threshold 3
      tab-always-indent 'complete)
;;
;; ======================================
;;; 자잘한 필요 모드
;; --------------------------------------
(save-place-mode 1)
(global-font-lock-mode 1)
(global-visual-line-mode t) ;word wrap
(global-auto-revert-mode 1)
(transient-mark-mode t)
(column-number-mode t)
(display-time-mode 1)
;;
;; ======================================
;;; modus theme
;; --------------------------------------
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-custom-auto-reload t 
	modus-themes-mode-line '(borderless)))
;;
;; ======================================
;;; start emacs (load theme by time)
;; --------------------------------------
;;
(defun set-theme-by-time ()
  "set theme by time"
  (let ((current-hour (string-to-number (substring (current-time-string) 11 13))))
    (if (and (>= current-hour 9) (< current-hour 17)) ; 9시부터 17시까지
        (load-theme 'modus-operandi)
      (load-theme 'modus-vivendi))))
;; display-time 모듈 사용. Emacs 시작 테마 설정.
(set-theme-by-time)
;;
;; ======================================
;;; exec-path-from-shell
;; --------------------------------------
;; MacOS PATH 설정
(use-package exec-path-from-shell
  :ensure t
  :if my-mactop-p  ;(eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize))
;;
;; ======================================
;;; Keyboard for MacOS
;; --------------------------------------
;; (when (equal system-type 'darwin)
;; ;;(when (string= system-name "MacBookAir.local")
;;   (setq mac-option-modifier 'super)
;;   (setq mac-command-modifier 'meta))
;;
;; ======================================
;;; 단축키 prefix key
;; --------------------------------------
;; 단축키 사용자 설정
(global-unset-key [f11])  ;remove toggle-frame-fullscreen/MacOS
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; M-x
(global-set-key (kbd "M-o") 'other-window)
;;
(defvar-keymap my-consult-map
  :doc "my consult map."
  "b" 'consult-bookmark
  "d" 'consult-dir
  "g" 'consult-grep
  "o" 'consult-outline
  "r" 'consult-recent-file
  "t" 'consult-theme)
;; -------------------------------------
(defvar-keymap my-prefix-map
  :doc "my prefix map."
  "c" my-consult-map
  "p" 'my-popmark
  "e" 'eshell
  "m" 'modus-themes-toggle
  "f" 'toggle-frame-fullscreen
  "o" 'org-custom-action
  "r" 'toggle-my-reading-mode
  "v" 'view-mode)
;;
(keymap-set global-map "C-t" my-prefix-map)
;; --------------------------------------
(defun my-popmark (choice)
  "Choices for directories and files."
  (interactive "c\[O]org(Dir) | [E]macs(Dir) | [P]pdf(Dir) | [i]nit | [t]asks | [c]Notes | [d]aily | [f]arm")
  (cond
   ((eq choice ?E)
    (if my-mactop-p
	(dired "~/Dropbox/emacs")
      (dired "~/emacs")))
   ((eq choice ?O)
    (if my-mactop-p
	(dired "~/Dropbox/eDoc/org")
      (dired "~/eDoc/org")))
   ((eq choice ?P)
    (if my-mactop-p
	(dired "~/Dropbox/eDoc/pdf")
      (dired "~/eDoc/pdf")))
   ((eq choice ?i)
    (if my-mactop-p
	(find-file "~/Dropbox/emacs/init.el")
      (find-file "~/emacs/init.el"))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?t)
    (if my-mactop-p
	(find-file "~/Dropbox/eDoc/org/Tasks.org")
      (find-file "~/eDoc/org/Tasks.org"))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?c)
    (if my-mactop-p
	(find-file "~/Dropbox/eDoc/org/cNotes.org")
      (find-file "~/eDoc/org/cNotes.org"))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?d)
    (if my-mactop-p
	(find-file "~/Dropbox/eDoc/org/Daily.org")
      (find-file "~/eDoc/org/Daily.org"))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?f)
    (if my-mactop-p
	(find-file "~/Dropbox/eDoc/org/dFarmNote.org")
      (find-file "~/eDoc/org/dFarmNote.org"))
    (message "Opened:  %s" (buffer-name)))
   (t (message "Quit"))))
;;
;; ======================================
;;; 로케일, 한글
;; --------------------------------------
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")		  ;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	  ;kbd 한글 S-SPC
;;
;; ======================================
;;; 글꼴 fonts
;; --------------------------------------
;; (set-frame-font "Noto Sans CJK KR" nil t)
;; (set-face-font 'fixed-pitch "Noto Sans Mono CJK KR")
(set-face-attribute 'default nil
		    :family "Hack" ;Hack, Menlo
		    :height 160)
(set-face-attribute 'fixed-pitch nil
		    :family "Noto Sans Mono CJK KR"
		    :height 160)
(set-face-attribute 'variable-pitch nil
		    :family "Noto Sans CJK KR"
		    :height 160)
(set-fontset-font t 'hangul (font-spec :family "Noto Sans Mono CJK KR"))
;;
;; ======================================
;;; korean calendar
;; --------------------------------------
(use-package calendar
  :config  
  (setq calendar-week-start-day 0
	calendar-day-name-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
;;	calendar-day-header-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"]))
;;
;;; calendar layout 보정. D2coding size
;; (defun cal-fixLayout ()
;;   (face-remap-add-relative 'default '(:family "D2Coding" :height 120)))           
;; (add-hook 'calendar-mode-hook 'cal-fixLayout)
;;
;; ======================================
;;; helpful
;; --------------------------------------
(use-package helpful
  :ensure t
  :bind(("C-h k" . helpful-key)
	("C-c C-d" . helpful-at-point)
	("C-h C" . helpful-command)
	("C-h o" . helpful-symbol)))
;;
;; ======================================
;;; recentF
;; --------------------------------------
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 50))
;;
;; ======================================
;;; org
;; --------------------------------------  
(use-package org
  :bind(("M-n" . 'outline-next-visible-heading)
	("M-p" . 'outline-previous-visible-heading)
	("C-0" . 'org-custom-action))
  :custom
  (org-startup-indented nil)            ;indent-mode enable
  (org-hide-leading-stars nil)          ;star invisible
  (org-startup-with-inline-images nil)  ;show inline images.(#+STARTUP: inlineimages)
  (org-adapt-indentation t)		;heading 이하 들여쓰기
  (org-src-preserve-indentation t)	;소스코드 여백 적용 export
;;  (org-agenda-start-with-log-mode t) ; agenda(ex 예정일과 완료일 구분 표시)
  (org-log-into-drawer t)               ; enable LOGBOOK drawer
  (org-log-done 'time)
  (org-image-actual-width '(100))       ; imagee 미리보기 사이즈
  :config
  (if my-laptop-p        ;(eq system-type 'gnu/linux)
      (setq org-directory (expand-file-name "~/eDoc/org/"))
    (setq org-directory (expand-file-name "~/Dropbox/eDoc/org/")))
  (setq org-agenda-files '("Tasks.org" "Daily.org"))
  (setq org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
;; capture
  (setq org-capture-templates
	'(("d" "Daily" entry (file+datetree "Daily.org") "* %?")
	  ("t" "Tasks" entry (file+olp "Tasks.org" "Schedule") "* TODO %?")
	  ("f" "dFarmNote" entry (file+datetree "dFarmNote.org") "* %?")))
;; export PDF
  (setq org-latex-title-command "\\maketitle \\newpage")
  (setq org-latex-toc-command "\\tableofcontents \\newpage")
  (setq org-latex-compiler "xelatex")
  (setq org-latex-to-pdf-process
	'("xelatex -interaction nonstopmode -output-directory %o %f"
	  "xelatex -interaction nonstopmode -output-directory %o %f"
	  "xelatex -interaction nonstopmode -output-directory %o %f")))
;;
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;
;; ======================================
;;; org-bullets
;; --------------------------------------
(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "●" "○" "●"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;;
;; ======================================
;;; for org edit/custom function
;; --------------------------------------
(defun org-custom-action (action)
  "Perform custom org-mode action based on the given ACTION.
   Possible values for ACTION:
    - 'newline': Insert a new line.
    - 'heading': Insert a new org-heading.
    - 'cycle': Insert a new paragraph and cycle visibility."
  (interactive
   (list (completing-read "Action: " '("1.newline" "2.heading" "3.cycle"))))
  (end-of-line)
  (cond
   ((equal action "1.newline") (newline-and-indent))
   ((equal action "2.heading") (org-insert-heading))
   ((equal action "3.cycle") (progn (newline-and-indent) (next-line) (org-cycle)))))
;; (defun org-custom-action ()
;;   "Perform custom org-mode action based on the user input."
;;   (interactive)
;;   (let ((action (read-string "Enter action (1: newline, 2: heading, 3: cycle): ")))
;;     (end-of-line)
;;     (cond
;;      ((string-equal action "1") (newline-and-indent))
;;      ((string-equal action "2") (org-insert-heading))
;;      ((string-equal action "3") (progn (newline-and-indent) (next-line) (org-cycle)))
;;      (t (message "Invalid action. Please enter 1, 2, or 3.")))
;;   ))
;; (global-set-key (kbd "C-c o") 'org-custom-action)

;;
;; ======================================
;;; which-key
;; --------------------------------------
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-setup-side-window-right))
;;
;; ======================================
;;; vertico
;; --------------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))
;;
;; ======================================
;;; marginalia
;; --------------------------------------
;; annotations in the minibuffer
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
;;
;; ======================================
;;; savehist
;; --------------------------------------
(use-package savehist
  :ensure t
  :init
  (savehist-mode))
;;
;; ======================================
;;; orderless
;; --------------------------------------
;; advenced completion stlye
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;;
;; ======================================
;;; consult
;; --------------------------------------
;; enhanced minibuffer commands, search
(use-package consult
  :ensure t
  :bind(("C-s" . consult-line)
	("C-x b" . consult-buffer)
	("C-x C-r" . consult-recent-file)
	:map minibuffer-local-map
        ("M-s" . consult-history)
        ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))
;;
;; ======================================
;;; consult-dir
;; --------------------------------------
;; insert paths into minibuffer prompts in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-c r d" . consult-dir)
         :map vertico-map
         ("C-c r d" . consult-dir)
         ("C-c r j" . consult-dir-jump-file)))
;;
;; ======================================
;;; embark
;; --------------------------------------
;; extended minibuffer actions and context menu
(use-package embark
  :ensure t
  :bind(("C-." . embark-act)              ; pick some comfortable binding
	("C-;" . embark-dwim)             ; good alternative: M-.
	("C-h B" . embark-bindings))      ; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Show Embark actions via which-key
  ;; https://config.daviwil.com/emacs
  (setq embark-action-indicator
	(lambda (map)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator))
;;
;; ======================================
;;; embark-consult
;; --------------------------------------
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;;
;; ======================================
;;; eradio
;; --------------------------------------
(use-package eradio
  :ensure t
  :init
  (cond
   (my-mactop-p   ;(eq system-type 'darwin)
    (setq eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc")))
   (my-laptop-p   ;(eq system-type 'gnu/linux)
    (setq eradio-player '("vlc" "--no-video" "-I" "rc"))))
  :config
  (setq eradio-channels '(("1.CBS Music FM" . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8")
			  ("2.BBS Radio" . "http://bbslive.clouducs.com:1935/bbsradio-live/livestream/playlist.m3u8")
			  ("3.BTN Woolim FM" . "rtmp://btn.nowcdn.co.kr/btn_ch4st/live.stream")
			  ("4.BTN BuddaMusic" . "rtmp://btn.nowcdn.co.kr/btn_ch3st/live.stream")
			  ("5.KBS Happy FM" . "http://serpent0.duckdns.org:8088/kbs2radio.pls")
			  ("6.KBS classic FM" . "http://serpent0.duckdns.org:8088/kbsfm.pls")
			  ("7.7080" . "http://wnffl.inlive.co.kr/live/listen.pls")
			  ("8.Trot" . "http://nest7942.inlive.co.kr/listen.pls"))))
;;
;; ======================================
;;; gnus
;; --------------------------------------
(setq user-mail-address "under9@icloud.com"
      user-full-name "Young")
;;
(setq gnus-select-method
      '(nnimap "icloud"
               (nnimap-address "imap.mail.me.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnmail-expiry-target "nnimap+icloud:Deleted Messages")
               (nnimap-authinfo-file "~/.authinfo")))
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.mail.me.com"
      smtpmail-smtp-server "smtp.mail.me.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      smtpmail-smtp-user "under9@icloud.com"
      smtpmail-debug-info t
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
;;
;; =======================================
;;; electric-pair-mode
;; ---------------------------------------
(progn
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\"))))
;;
;; =======================================
;;; corfu
;; ---------------------------------------
(use-package corfu
  ;; TAB-and-Go customizations
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
;;  (completion-styles '(basic)) ;consult-line err
  (corfu-preselect 'prompt)   ;; Always preselect the prompt
  (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)       ;; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
;; (corfu-quit-at-boundary 'separator)
;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
	("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))
;;
;; =======================================
;;; all-the-icons
;; ---------------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
;;
;; dired with icons
(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode))
;;
;; from https://kristofferbalintona.me/posts/202202211546/
(use-package all-the-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
;;
;; ======================================
;;; dired
;; --------------------------------------
;; directory 우선/한글파일명 순 정렬불가(macOS)
;; → 해결(setenv "LC_COLLATE" "C")
(use-package dired
  :preface
  (defun sof/dired-sort ()
  "Dired sort, directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'emacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
  ;; -------------------------------------
  (defun my/dired-jump-to-top()
    "Dired, jump to top"
    (interactive)
    (goto-char (point-min))
    (dired-next-line 2))
  ;; -------------------------------------
  (defun my/dired-jump-to-bottom()
    "Dired, jump to bottom"
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  :config
  (setq dired-auto-revert-buffer t)
  :bind (:map dired-mode-map
	      ("M-<up>" . my/dired-jump-to-top)
	      ("M-<down>" . my/dired-jump-to-bottom)
	      ("/" . dired-narrow)
	      ("C-c f" . toggle-frame-fullscreen)
	      ("<tab>" . dired-subtree-toggle)
	      ("<backtab>" . dired-subree-cycle)))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)
;;
;; ======================================
;;; dired-narrow
;; --------------------------------------
;; Dired 모드에서 파일 목록 필터링
(use-package dired-narrow :ensure t
  :after dired)
;;
;; ======================================
;; dired-subtree
;; --------------------------------------
;; Tab. sub directory 표시
(use-package dired-subtree :ensure t
  :after dired)
;;
;; ======================================
;;; eshell
;; --------------------------------------
(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))
;;
;; ======================================
;;; modeline
;; --------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-nil) ;display full path
  (when my-laptop-p    ;(eq system-type 'gnu/linux)
    (setq doom-modeline-icon nil)))
;;
;; ======================================
;;; denote
;; --------------------------------------
(use-package denote
  :ensure t
  :bind(("C-c n n" . denote)
	("C-c n s" . denote-sort-dired))
  :config
  ;; Remember to check the doc strings of those variables.
  (if my-mactop-p
      (setq denote-directory (expand-file-name "~/Dropbox/eDoc/org/denote/"))
    (setq denote-directioy (expand-file-name "~/eDoc/org/denote/")))
;;  (setq denote-known-keywords '("emacs" "latex" "idea"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-date-format nil) ; read doc string
  (setq denote-backlinks-show-context t)
  ;;
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New Denote" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))))
;;
;; ======================================
;;; rainbow-delimiters
;; --------------------------------------
;; 괄호, 중괄호, 각종 쌍을 시각적(무지개색) 구분
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
	  '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
	  '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
	  '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
	  '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
	  '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
	  '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
	  '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
	  '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))))
;;
;; ======================================
;;; view-mode
;; --------------------------------------
;; 읽기 모드, 편집 보호
(use-package view
  :ensure nil       ; built-in
  :init
  (setq view-read-only t)
  :bind
  (:map view-mode-map
	("n" . next-line) ;move-down
	("p" . previous-line)))
;;
;; ======================================
;;; pdf-view, tools
;; --------------------------------------
;; only linux
(use-package pdf-tools
  :ensure nil
  :if my-laptop-p   ;;(eq system-type 'gnu/linux)
  :mode ("\\.pdf\\'" . pdf-view-mode) ; Automatically open PDFs in pdf-view-mode
  :config
  (setq pdf-view-display-size 'fit-width) ; Set the default zoom level
  (pdf-tools-install))
;;
;; ======================================
;;; exwm
;; --------------------------------------
;; only linux
;; (if my-laptop-p
;;     (require 'exwm)
;;   (require 'exwm-config)
;;   (exwm-config-default)
;;   ;;Super + R로 EXWM을 재설정하고, Super + W로 워크스페이스를 전환
;;   (setq exwm-input-global-keys
;;       `(([?\s-r] . exwm-reset)
;;         ([?\s-w] . exwm-workspace-switch)
;;         ;; 추가적인 키 바인딩 설정
;;         ))
;; )
;;
;; ======================================
;;; my-reading-mode
;; -------------------------------------
;; 읽기 모드, 쓰기 금지
(defun toggle-my-reading-mode ()
  "Toggle fullscreen & view-mode."
  (interactive)
  (if (and (boundp 'my-reading-mode-enabled) my-reading-mode-enabled)
      (progn
        (toggle-frame-fullscreen)
        (text-scale-decrease 0.5)
        (setq my-reading-mode-enabled nil)
        (view-mode -1))
    (progn
      (toggle-frame-fullscreen)
      (text-scale-increase 0.5)
      (setq my-reading-mode-enabled t)
      (view-mode))))
