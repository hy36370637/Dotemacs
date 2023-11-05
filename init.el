;; ===============================================================
;;; macbook air & macMini
;;================================================================
;; (require 'org)
;; (org-babel-load-file
;;  (expand-file-name "myAirinit.org" user-emacs-directory))

;; ======================================
;;; system Mac
;; --------------------------------------
;; (defvar myMacbookAir (equal (system-name) "MacBookAir.local"))
;; (defvar myMacMini (equal (system-name) "Macmini2.local"))

;; ======================================
;;; package source list
;; --------------------------------------
(require 'package)
;; https://sachachua.com/dotemacs/index.html
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
(package-initialize)

;; ======================================
;;; use-package
;; --------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; ======================================
;;; window. general
;; --------------------------------------
;; menu Bar *******
(menu-bar-mode 1)
(tool-bar-mode -1)                                 ; hide tool-bar
(toggle-scroll-bar -1)                             ; toggle scroll-bar(hide)

;; 시작메시지 없애기 *******
(setq inhibit-startup-message t
      visible-bell t)
(setq initial-scratch-message nil)
(setq frame-title-format "| dole's Emacs | %b |")  ; display window title
(setq kill-whole-line 1)                           ; 한 줄 삭제(C-S-backspace)
;; (setq-default line-spacing 8)                      ; line spacing
(setq search-highlight t)                          ; search highlight
;; (setq calc-group-digits t)                         ; 계산기 자릿수 분리기호 삽입
(setq default-directory "~/Dropbox/eDoc/")          ; default directory
(setq use-dialog-box nil)                          ; Don't pop up UI dialogs when prompting
(setq global-auto-revert-non-file-buffers t)	   ; Revert Dired and other buffers
;; (setq make-backup-files nil)                    ; don't backup files
;; (setq next-line-add-newlines nil)                  ; blank line add nil
;; (setq next-screen-context-lines 3)                 ; 페이지 넘길때 3줄 포함
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))    ; backup 디렉토리
(fset 'yes-or-no-p 'y-or-n-p)                      ; change "yes or no" to "y or n"

;; 필요 모드 *******
(save-place-mode 1)                                 ; last cursor location of opened files
(global-font-lock-mode 1)                           ; Syntax Highlighting
(global-hl-line-mode t)                             ; Highlight cursor line
(global-visual-line-mode t)                         ; enable word wrap
(global-auto-revert-mode 1)                         ; Revert buffers when the underlying file has changed
;; (display-time-mode 1)                               ; display time, clock
(transient-mark-mode t)                             ; show mark range
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode t))        	            ; > emacs29

;; orgmode 시작
(setq initial-major-mode 'org-mode)                 ; initial mode 'org'
;; staring theme
;; (when window-system
;;   (load-theme 'modus-operandi t))                   ; default theme
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "C-z") 'undo)

;; Size of the starting Window
;; (if myMacbookAir
;;     (setq initial-frame-alist '((top . 1) (left . 240) (width . 101) (height . 70)))
;;   (setq initial-frame-alist '((top . 1) (left . 500) (width . 101) (height . 70))))

;; ======================================
;;; exec-path-from-shell
;; --------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; ======================================
;;; full screen
;; --------------------------------------
;; toggle fullscreen
;; https://superuser.com/questions/256404/fullscreen-emacs-in-osx
(defun toggle-fullscreen (&optional f)
   (interactive)
   (let ((current-value (frame-parameter nil 'fullscreen)))
     (set-frame-parameter nil 'fullscreen
			  (if (equal 'fullboth current-value)
			      (if (boundp 'old-fullscreen) old-fullscreen nil)
			    (progn (setq old-fullscreen current-value)
				   'fullboth)))))
;; (global-set-key (kbd "C-c t f") 'toggle-fullscreen)

;; ======================================
;;; for org edit/custom function
;; --------------------------------------
;;; insert new line below current line
(defun inNewline ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; insert new heading
(defun org_New_Heading ()
  (interactive)
  (end-of-line)
  (org-insert-heading))

;;; insert paragraph, <tab> orgcle
(defun newOrgcyc ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (next-line)
  (org-cycle))

;; ======================================
;;; move line
;; --------------------------------------
;; move the current line up.
(defun move-line-up()
  "move the currnet line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;; move the current line down
(defun move-line-down()
  "move the currnet line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; (defun insert_today ()
;;; insert today
;;   "Insert Date YYYY-MM-DD"
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d")))

;;; config init
;; https://github.com/daedreth/UncleDavesEmacs
;; (defun config-visit ()
;;   "config myinit"
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el"))
;; (global-set-key (kbd "C-c t e") 'config-visit)


;;; select current line
;; http://xahlee.info/emacs/emacs/modernization_mark-word.html
;; (defun select-current-line ()
;;   "Select current line. If region is active, extend selection downward by line"
;;   (interactive)
;;   (if (region-active-p)
;;       (progn
;; 	(forward-line 1)
;; 	(end-of-line))
;;     (progn
;;       (end-of-line)
;;       (set-mark (line-beginning-position)))))

;; ======================================
;;; new blank buffer
;; --------------------------------------
;; https://ders45.blogspot.com/2016/04/emacs.html
;; (defun new-empty-buffer ()
;;   "Open a New empty buffer."
;;   (interactive)
;;   (let ((buf (generate-new-buffer "New empty")))
;;     (switch-to-buffer buf)
;;     (funcall (and initial-major-mode))
;;     (setq buffer-offer-save t)))

;; ======================================
;;; locale. korean
;; --------------------------------------
;; https://www.reddit.com/r/emacs/comments/siuvpu/isnt_there_a_better_way_to_set_utf8/
;; ------------
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")		;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	;kbd 자동 S-SPC

;; ======================================
;;; 글꼴, 한글
;; --------------------------------------
(set-face-attribute 'default nil :family "Hack" :height 150)   ;기본글꼴 IntleOneMono-Reqular
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 150)
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))    ;Apple SD 산돌고딕 Neo/D2Coding

;; ======================================
;;; korean calendar
;; --------------------------------------
(use-package calendar
  :ensure nil
  :config  
  (setq calendar-week-start-day 1
	calendar-day-name-array ["일" "월" "화" "수" "목" "금" "토"]
	calendar-day-header-array ["일" "월" "화" "수" "목" "금" "토"]
	calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"])
  ;; (setq calendar-holidays korean-holidays)                    ;; korean holidays
  :custom
  (calendar-mark-holidays-flag t))

;;; Calendar layout 보정. D2coding size
(defun set-cal-fixLayout ()
  (face-remap-add-relative 'default '(:family "D2Coding" :height 120)))           
(add-hook 'calendar-mode-hook 'set-cal-fixLayout)

;; ======================================
;;; hydra
;; --------------------------------------
(use-package hydra
   :ensure t
   :commands defhydra
   :bind (("C-1" . 'hydra-default/body)
	  ("C-2" . 'hydra-org/body)))
(use-package use-package-hydra)

;; ======================================
;;; custom defhydra
;; --------------------------------------
(with-eval-after-load 'hydra
  
  (defhydra hydra-default (:color pink
  	 		   :hint none
			   :exit t)
 "
^Launch^        ^Moves^           ^Actions^          ^Radio^
^^^^^^^^----------------------------------------------------------------
_a_: agenda     _p_: lineUP     _t_: toggle-item   _P_: Play
_c_: config     _n_: lineDown   _i_: imenu         _S_: Stop
_d_: dayNote    _T_: theme      _F_: fullScreen     ^ ^  
_e_: eshell      ^ ^              ^ ^               ^ ^
 "
    ("a" (find-file "~/Dropbox/eDoc/org/Agenda.org")  "agenda")
    ("c" (find-file "~/.emacs.d/init.el")  "config")
    ("d" (find-file "~/Dropbox/eDoc/org/dayNotes.org")  "dayNotes")
    ("e" eshell "eshell")
    ("p" move-line-up "lineUp")
    ("n" move-line-down "lineDown")
    ("t" org-toggle-item "toggle-item")
    ("i" consult-imenu "imenu")
    ("F" toggle-fullscreen "fullScreen")
    ("P" eradio-play "PlayR")
    ("S" eradio-stop "StopR")
    ("T" consult-theme "Theme")
    ;; ("L" hydra-launch/body "launch/fn")
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("v" Buffer-menu-select "select" :color blue)
    ("q" nil "cancel" :color blue)))

  ;; (defhydra hydra-launch (:exit t)
  ;;   "launch/fn"
    ;; ("a" (find-file "~/Dropbox/eDoc/org/Agenda.org")  "agenda")
    ;; ("c" (find-file "~/.emacs.d/init.el") "init")
    ;; ("d" (find-file "~/Dropbox/eDoc/org/cDongSang.org")  "cDongsang")
    ;; ("e" eshell "eshell")
    ;; ("f" find-file "file")
    ;; ("g" (browse-url "https://github.com/ushy6532/emacs.d") "github")
    ;; ("n" new-empty-buffer "newbuffer")
    ;; ("s" select-current-line "sel-line")
    ;; ("u" my-url-search "Search")
    ;; ("T" consult-theme "Theme")
    ;; ("F" toggle-fullscreen "fullScreen")
    ;; ("D" hydra-default/body "default/R")
    ;; ("q" nil "cancel")))
;;  (global-set-key (kbd "<f5>") #'hydra-launch/body))

;; ======================================
;;; helpful
;; --------------------------------------
(use-package helpful
  :bind
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h C" . helpful-command)
  ("C-h o" . helpful-symbol)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; ======================================
;;; recentF
;; --------------------------------------
(use-package recentf
;;   :ensure t
;;   :bind ("C-x C-r" . 'recentf-open-files)
  :config
  (recentf-mode 1))

;; ======================================
;;; orgmode
;; --------------------------------------
(use-package org
  :ensure t
  :custom
  (org-startup-indented nil)                                ; indent
  (org-hide-leading-stars nil)
  (org-startup-with-inline-images nil)                      ; show inline images.(#+STARTUP: inlineimages)
  ;; (org-image-actual-width 600)
  ;; (org-latex-images-centered nil)                        ; default t
  (org-adapt-indentation t)
  (org-src-preserve-indentation nil)
  ;; (org-src-tab-acts-natively t)	                      ; default t
  ;; (org-edit-src-content-indentation 2)
  (org-log-into-drawer t)                                   ; enable LOGBOOK drawer
  ;; (org-log-state-notes-insert-after-drawers nil)
  ;; (org-use-speed-command t)                              ; enable speed keys
  (org-log-done 'time)				         ; TODO. Closing items
  ;; (org-log-done 'note)
  :config
  (setq org-directory (expand-file-name "~/Dropbox/eDoc/org"))    ; org default directory
  (setq org-agenda-files '("Agenda.org" "dayNotes.org"))        ; agenda files
  (setq org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))   ; shift-F(follow-mode, move key F,B)
;; capture
  (setq org-capture-templates
	'(("n" "dayNote" entry (file+datetree "dayNotes.org") "* %?")
	  ("f" "dFarmNote" entry (file+datetree "dFarmNote.org") "* %?")))
;; for export PDF
;; https://emacs.stackexchange.com/questions/42558/org-mode-export-force-page-break-after-toc/42579
(setq org-latex-title-command "\\maketitle \\newpage")
(setq org-latex-toc-command "\\tableofcontents \\newpage")
(setq org-latex-compiler "xelatex")                  ;; use xelatex
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
	"xelatex -interaction nonstopmode -output-directory %o %f"
	"xelatex -interaction nonstopmode -output-directory %o %f")))

;; shotkeys in orgmode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-x r") 'org-mark-ring-goto)
	    (local-set-key (kbd "M-n") 'outline-next-visible-heading)
	    (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
	    ;; (local-set-key (kbd "C-c a") 'org-agenda)
	    ;; (local-set-key (kbd "C-c c") 'org-capture)
	    (local-set-key (kbd "C-c i") 'org-toggle-item)
	    (local-set-key (kbd "C-0") 'inNewline)
	    (local-set-key (kbd "C-9") 'newOrgcyc)
	    (local-set-key (kbd "C-8") 'org_New_Heading)))
;; latex-class
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
                '("obchapter"
                  "\\documentclass{oblivoir}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; hydra
(with-eval-after-load 'hydra
  (defhydra hydra-org (:exit t)
    "hydra-org"
    ("a" org-agenda "agenda")
    ("b" org-mark-ring-goto  "ring-goto")
    ("c" org-capture "capture")
    ("e" org-emphasize "org-emphasize")
    ("f" org-footnote-new "footnote")
    ("n" outline-next-visible-heading  "next-heading")
    ("p" outline-previous-visible-heading "previous-heading")
    ("t" org-toggle-item "toggle-item")
    ("0" inNewline "inNewline")
    ("9" newOrgcyc "newOrgcyc")
    ("8" org_New_Heading "org_New_head")
    ("q" nil "cancel")))

;; ======================================
;;; os-mx
;; --------------------------------------
;; export Markdown
;; (use-package ox-md
;;   :ensure org
;;   :after (org))

;; ======================================
;;; org-bullets
;; --------------------------------------
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ======================================
;;; yasnippet
;; --------------------------------------
(use-package yasnippet
  :ensure t 
  :config
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  (yas-global-mode 1))
;;   (yas-reload-all)
;;   (add-hook 'org-mode-hook #'yas-minor-mode))

;; ======================================
;;; which-key
;; --------------------------------------
(use-package which-key
    :ensure t
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.2))

;; ======================================
;;; doom modeline
;; --------------------------------------
;; (use-package doom-modeline
;;   :ensure t
;;   :after all-the-icons
;;   :init (doom-modeline-mode t))

;; ======================================
;;; vertico
;; --------------------------------------
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; ======================================
;;; marginalia
;; --------------------------------------
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; ======================================
;;; savehist
;; --------------------------------------
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; ======================================
;;; orderless
;; --------------------------------------
;; ordeless completion stye
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	complition-category-overrides '((file (styles . (partial-completion))))))

;; ======================================
;;; consult
;; --------------------------------------
;; enhanced minibuffer commands
(use-package consult
  :ensure t
  :bind(("C-s" . consult-line)
	("C-c i" . consult-imenu)
	("C-c t" . consult-theme)
	("C-x C-r" . consult-recent-file)
	("C-x b" . consult-buffer)
	:map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history)))                ; orig. switch-to-buffer

;; ======================================
;;; embark
;; --------------------------------------
;; extended minibuffer actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)              ; pick some comfortable binding
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ======================================
;;; eradio
;; --------------------------------------
(use-package eradio
  :ensure t
  :init
  (setq eradio-show-nowplaying t)
  (setq eradio-channels '(("1.CBS Music FM" . "http://cbs.co.kr")
			  ("2.BBS Radio" . "http://bbslive")
			  ("3.BTN Woolim FM" . "rtmp://btn.")
			  ("4.BTN BuddaMusic" . "rtmp://btn.n")))
  (setq eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc")))

;; ======================================
;;; gnus
;; --------------------------------------
(setq user-mail-address "hy36370637@gmail.com"
      user-full-name "김호영")
(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it)
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ;; imap.naver.com
	       (nnimap-server-port "imaps") 
	       (nnimap-stream ssl)))
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; =======================================
;;; electric-pair-mode
;; =======================================
;; http://ohyecloudy.com/emacsian/2020/08/23/package-show-paren-mode-electric-pair-mode/
(progn
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")))
  )

;; =========================================
;;; company
;; =========================================
;; (use-package company
;;   :ensure t
;;   :init
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   :config
;;   (setq company-backends '(company-elisp company-capf company-yasnippet))
;;   (setq company-idle-delay 0.2)              ; Show suggestions after a short delay
;;   (setq company-minimum-prefix-length 2)     ; Show suggestions after typing at least 2 characters
;;   (setq company-tooltip-limit 10)            ; Show maximum 10 suggestions
;;   :bind(:map company-active-map
;; 	     ("C-n" . company-select-next)
;; 	     ("C-p" . company-select-previous)))

;; ;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
;; (defun trigger-org-company-complete ()
;;   "Begins company-complete in org-mode buffer after pressing #+ chars."
;;   (interactive)
;;   (if (string-equal "#" (string (preceding-char)))
;;     (progn
;;       (insert "+")
;;       (company-complete))
;;     (insert "+")))

;; (eval-after-load 'org '(define-key org-mode-map
;; 			 (kbd "+") 'trigger-org-company-complete))

;; =========================================
;;; all-the-icons
;; =========================================
(use-package all-the-icons)

;; dired with icons
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; from https://kristofferbalintona.me/posts/202202211546/
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; ======================================
;;; dired
;; --------------------------------------
;; https://ders45.blogspot.com/2016/04/emacs.html
;; directory 우선/ 한글파일명 정렬안됨(macOS) → 해결(setenv "LC_COLLATE" "C")
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)

;; ======================================
;;; ace-window
;; --------------------------------------
(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

;; ==========================================
;;; magit
;; ==========================================
(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind (("C-x g" . magit-status)))

;; ======================================
;;; eshell
;; --------------------------------------
(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))


;; ===========================================
;;; custom file
;; ===========================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
