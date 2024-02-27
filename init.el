;; ======================================
;;; Speed up emacs
;; --------------------------------------
;; Decrease the number of garbage collection invocations
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
;; (setq frame-title-format "| dole's Emacs | %b |")
;;
;; ======================================
;;; 작은 설정 들
;; --------------------------------------
;; (setq global-auto-revert-non-file-buffers t)
(setq default-directory "~/Dropbox/eDoc/org/")
(setq temporary-file-directory "~/Dropbox/eDoc/tmpdir/") ;temp dir
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq make-backup-files nil
      kill-whole-line 1
      search-highlight t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default line-spacing 0.2)    ; 줄 간격 1.5
;;; for corfu
(setq completion-cycle-threshold 3
      tab-always-indent 'complete)
;;
;; ======================================
;;; 자잘한 필요 모드
;; --------------------------------------
(save-place-mode 1)
(global-font-lock-mode 1)
(global-visual-line-mode t)
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
	modus-themes-mode-line '(borderless))
  (load-theme 'modus-vivendi))
;;
;; ======================================
;;; exec-path-from-shell
;; --------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))
;;
;; ======================================
;;; prefix key
;; --------------------------------------
(global-unset-key [f11])  ;remove toggle-frame-fullscreen/MacOS
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; M-x
(global-set-key (kbd "M-o") 'other-window)
(defvar-keymap my-prefix-map
  :doc "my prefix map."
  "t" 'my-popmark
  "e" 'eshell
  "m" 'modus-themes-toggle
  "f" 'toggle-frame-fullscreen
  "g" 'consult-grep
  "p" 'eradio-play
  "s" 'eradio-stop)
(keymap-set global-map "C-t" my-prefix-map)
;;
(defun my-popmark (choice)
  ;;https://stackoverflow.com/questions/19283368/how-can-i-open-quickly-a-file-in-emacs/19284395#19284395
  "Choices for directories and files."
  (interactive "c\[O]rg | [E]macs | [P]df | [i]nit | [t]asks | [c]Notes | [d]aily | [f]arm")
  (cond
   ((eq choice ?O)
    (dired "~/Dropbox/eDoc/org"))
   ((eq choice ?E)
    (dired "~/Dropbox/emacs"))
   ((eq choice ?P)
    (dired "~/Dropbox/eDoc/pdf"))
   ((eq choice ?i)
    (find-file "~/Dropbox/emacs/init.el")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?t)
    (find-file "~/Dropbox/eDoc/org/Tasks.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?c)
    (find-file "~/Dropbox/eDoc/org/cNotes.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?d)
    (find-file "~/Dropbox/eDoc/org/Daily.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?f)
    (find-file "~/Dropbox/eDoc/org/dFarmNote.org")
    (message "Opened:  %s" (buffer-name)))
   (t (message "Quit"))))
;;
;; ======================================
;;; locale. korean
;; --------------------------------------
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")		  ;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	  ;kbd 한글 S-SPC
;;
;; ======================================
;;; font
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
;; (set-fontset-font t 'hangul (font-spec :family "Noto Sans Mono CJK KR")) ;D2Coding, Apple SD 산돌고딕 Neo
;;
;; ======================================
;;; korean calendar
;; --------------------------------------
(use-package calendar
  :config  
  (setq calendar-week-start-day 0
	;; calendar-day-name-array ["일" "월" "화" "수" "목" "금" "토"]
	;; calendar-day-header-array ["일" "월" "화" "수" "목" "금" "토"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월"
				   "10월" "11월" "12월"])
  ;; (setq calendar-holidays korean-holidays)   ;; package-install korean-holidays
  :custom
  (calendar-mark-holidays-flag nil))
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
;; (use-package recentf
;;   :ensure t
;;   :config
;;   (recentf-mode 1))
;;
;; ======================================
;;; org
;; --------------------------------------  
(use-package org
  :bind(("M-n" . 'outline-next-visible-heading)
	("M-p" . 'outline-previous-visible-heading)
	("C-0" . 'inNewline)
	("C-9" . 'newOrgcyc)
	("C-8" . 'org_New_Heading))
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
  (setq org-directory (expand-file-name "~/Dropbox/eDoc/org/"))
  (setq org-agenda-files '("Tasks.org" "Daily.org"))
  (setq org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))   ; shift-F(follow-mode, move key F,B)
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
(defun inNewline ()
  "new line, below current line"
  (interactive)
  (end-of-line)
  (newline-and-indent))
;;
(defun org_New_Heading ()
  "new org-insert-heading"
  (interactive)
  (end-of-line)
  (org-insert-heading))
;;
(defun newOrgcyc ()
  "new paragraph,org-cycle"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (next-line)
  (org-cycle))
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
	("C-c r b" . consult-bookmark)
	("C-c r g" . consult-grep)
	("C-c r o" . consult-outline)
	("C-c r t" . consult-theme)
;;	("C-x C-r" . consult-recent-file)
	("C-x b" . consult-buffer)
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
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
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
  (setq eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc"))
  :config
;;  (setq eradio-show-nowplaying t)
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
  :if (display-graphic-p))
;;
;; dired with icons
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode))
;;
;; from https://kristofferbalintona.me/posts/202202211546/
(use-package all-the-icons-completion
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
  :config
  (setq dired-auto-revert-buffer t))
;;
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
(add-hook 'dired-after-readin-hook 'sof/dired-sort)
;;
;; ======================================
;;; dired-narrow
;; --------------------------------------
;; Dired 모드에서 파일 목록 필터링
(use-package dired-narrow
  :ensure t
  :bind
  (:map dired-mode-map
	("C-c C-n". dired-narrow)))
;;
;; ======================================
;;; dired-subtree
;; --------------------------------------
;; Dired에서 디렉터리 확장, 축소
(use-package dired-subtree
  :ensure nil
  :bind
  (:map dired-mode-map
	([tab] . dired-subtree-toggle)
	([backtab] . dired-subtree-cycle)))
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
  (setq doom-modeline-icon t))
;;
;; ======================================
;;; denote
;; --------------------------------------
(use-package denote
  :bind(("C-c n n" . denote)
	("C-c n s" . denote-sort-dired))
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Dropbox/eDoc/org/denote/"))
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
;; 괄호, 중괄호, 각종 쌍을 시각적으로 구분
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
