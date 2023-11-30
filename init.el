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
;;; custom file
;; --------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (when (file-exists-p custom-file) (load custom-file 't))

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
(setq use-package-always-ensure nil)

;; ======================================
;;; window. general
;; --------------------------------------
;; menu Bar
(menu-bar-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; 시작메시지 없애기
(setq inhibit-startup-message t
      visible-bell t)
(setq initial-scratch-message nil)

;; default
(setq frame-title-format "| dole's Emacs | %b |")
(setq kill-whole-line 1)
(setq search-highlight t)
(setq use-dialog-box nil)
(setq global-auto-revert-non-file-buffers t)
(setq apropos-sort-by-scores t)   ;apropos 관련순 정렬
;; (setq custom-file (make-temp-file "emacs-custom")) ; custom.el 생성
(setq default-directory "~/Dropbox/eDoc/")
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(fset 'yes-or-no-p 'y-or-n-p)
;;corfu
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
;; 필수 모드
(save-place-mode 1)
(global-font-lock-mode 1)
(global-visual-line-mode t)
(global-auto-revert-mode 1)
(transient-mark-mode t)
;; (column-number-mode t)
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode t))

;; START orgmode
;; (setq initial-major-mode 'org-mode)
;; START theme
;; (when window-system
;;   (load-theme 'modus-operandi t))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "C-z") 'undo)

;; for mac
;; (when system-type 'darwin
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-option-modifier 'super))
;; ======================================
;;; Size of the starting Window 
;; --------------------------------------
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
;;; toggle full screen
;; --------------------------------------
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
;; insert new line below current line
(defun inNewline ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; insert new heading
(defun org_New_Heading ()
  (interactive)
  (end-of-line)
  (org-insert-heading))

;; insert paragraph, <tab> orgcle
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

;; ======================================
;;; locale. korean
;; --------------------------------------
;; https://www.reddit.com/r/emacs/comments/siuvpu/isnt_there_a_better_way_to_set_utf8/
;; ------------
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_COLLATE" "C")		;Dired 한글 파일명 정렬 macOS
(set-locale-environment "ko_KR.UTF-8")	;kbd 한글 S-SPC

;; ======================================
;;; 글꼴, 한글
;; --------------------------------------
(set-face-attribute 'default nil :family "Hack" :height 150)
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 150)
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))

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
  (setq calendar-holidays korean-holidays)   ;; package-install korean-holidays
  :custom
  (calendar-mark-holidays-flag nil))

;;; Calendar layout 보정. D2coding size
(defun cal-fixLayout ()
  (face-remap-add-relative 'default '(:family "D2Coding" :height 120)))           
(add-hook 'calendar-mode-hook 'cal-fixLayout)

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
_a_: agenda     _p_: lineUP     _D_: Dired         _P_: Play
_c_: config     _n_: lineDown   _i_: imenu         _S_: Stop
_d_: dayNote    _T_: theme      _o_: outline        ^ ^
_e_: eshell     _f_: farmNote   _F_: FullSCR        ^ ^
 "
    ("a" (find-file "~/Dropbox/eDoc/org/Agenda.org")  "agenda")
    ("c" (find-file "~/.emacs.d/init.el")  "config")
    ("d" (find-file "~/Dropbox/eDoc/org/dayNotes.org")  "dayNote")
    ("f" (find-file "~/Dropbox/eDoc/org/dFarmNote.org") "farmNote")
    ("e" eshell "eshell")
    ("o" consult-outline "outline")
    ("p" move-line-up "lineUp")
    ("n" move-line-down "lineDown")
    ("i" consult-imenu "imenu")
    ("D" dired-other-window "Dired")
    ("F" toggle-fullscreen "FullSCR")
    ("P" eradio-play "PlayR")
    ("S" eradio-stop "StopR")
    ("T" consult-theme "Theme")
    ("q" nil "cancel" :color blue)))

;; ======================================
;;; helpful
;; --------------------------------------
(use-package helpful
  :bind(("C-h k" . helpful-key)
	("C-c C-d" . helpful-at-point)
	("C-h C" . helpful-command)
	("C-h o" . helpful-symbol))
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
  (org-startup-indented nil)   ;indent-mode enable
  (org-hide-leading-stars nil) ;star invisible
  (org-startup-with-inline-images nil)  ;show inline images.(#+STARTUP: inlineimages)
  ;; (org-image-actual-width 600)
  ;; (org-latex-images-centered nil)    ;default t
  (org-adapt-indentation t)		;heading 이하 들여쓰기
  (org-src-preserve-indentation t)	;소스코드 여백 적용 export
  ;; (org-edit-src-content-indentation 4)
  (org-log-into-drawer t)               ; enable LOGBOOK drawer
  ;; (org-log-state-notes-insert-after-drawers nil)
  (org-log-done 'time)
  (org-support-shift-select t)
  :config
  (setq org-directory (expand-file-name "~/Dropbox/eDoc/org"))
  (setq org-agenda-files '("Agenda.org" "dayNotes.org"))
  (setq org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))   ; shift-F(follow-mode, move key F,B)
;; capture
  (setq org-capture-templates
	'(("n" "dayNote" entry (file+datetree "dayNotes.org") "* %?")
	  ("f" "dFarmNote" entry (file+datetree "dFarmNote.org") "* %?")))
;; export PDF
  (setq org-latex-title-command "\\maketitle \\newpage")
  (setq org-latex-toc-command "\\tableofcontents \\newpage")
  (setq org-latex-compiler "xelatex")
  (setq org-latex-to-pdf-process
	'("xelatex -interaction nonstopmode -output-directory %o %f"
	  "xelatex -interaction nonstopmode -output-directory %o %f"
	  "xelatex -interaction nonstopmode -output-directory %o %f")))
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
;; (with-eval-after-load 'ox-latex
;;   (add-to-list 'org-latex-classes
;;                '("obchapter"
;;                  "\\documentclass{oblivoir}"
;;                  ("\\chapter{%s}" . "\\chapter*{%s}")
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'hydra
  (defhydra hydra-org (:exit t)
    "hydra-org"
    ("a" org-agenda "agenda")
    ("b" org-mark-ring-goto  "ring-goto")
    ("c" org-capture "capture")
    ("e" org-emphasize "org-emphasize")
    ("f" org-footnote-new "footnote")
    ("l" org-store-link "org-store-link" )
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
;;; org-download
;; --------------------------------------
;; (use-package org-download
;;   :config
;;   (setq org-download-method 'directory)
;;   (setq org-download-image-dir "~/Dropbox/eDoc/org/img/org-dn-img"))
;;  (setq org-download-image-html-width 600))

;; ======================================
;;; yasnippet
;; --------------------------------------
(use-package yasnippet
  :ensure t 
  :config
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  (yas-global-mode 1))

;; ======================================
;;; which-key
;; --------------------------------------
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-setup-side-window-right))
  ;; (setq which-key-popup-type 'frame)

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ======================================
;;; consult
;; --------------------------------------
;; enhanced minibuffer commands
(use-package consult
  :ensure t
  :bind(("C-s" . consult-line)
	("C-c i" . consult-imenu)
	("C-c o" . consult-outline)
	("C-c t" . consult-theme)
	("C-x C-r" . consult-recent-file)
	("C-x b" . consult-buffer)
	:map minibuffer-local-map
        ("M-s" . consult-history)
        ("M-r" . consult-history)))

;; ======================================
;;; embark
;; --------------------------------------
;; extended minibuffer actions
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
  (setq eradio-channels '(("1.CBS Music FM" . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8")
			  ("2.BBS Radio" . "http://bbslive.clouducs.com:1935/bbsradio-live/livestream/playlist.m3u8")
			  ("3.BTN Woolim FM" . "rtmp://btn.nowcdn.co.kr/btn_ch4st/live.stream")
			  ("4.BTN BuddaMusic" . "rtmp://btn.nowcdn.co.kr/btn_ch3st/live.stream")
			  ("5.KBS Happy FM" . "http://serpent0.duckdns.org:8088/kbs2radio.pls")
			  ("6.KBS classic FM" . "http://serpent0.duckdns.org:8088/kbsfm.pls")
			  ("7.7080" . "http://wnffl.inlive.co.kr/live/listen.pls")
			  ("8.Trot" . "http://nest7942.inlive.co.kr/listen.pls")))
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
;; ---------------------------------------
;; http://ohyecloudy.com/emacsian/2020/08/23/package-show-paren-mode-electric-pair-mode/
(progn
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\"))))

;; =======================================
;;; corfu
;; ---------------------------------------
(use-package corfu
;;  TAB-and-Go customizations
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
;;  (completion-styles '(basic)) ;consult-line err
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s) ;; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
;;  (corfu-quit-at-boundary 'separator)
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

;; =======================================
;;; dabbrev
;; ---------------------------------------
;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; =======================================
;;; company
;; ---------------------------------------
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

;; =======================================
;;; all-the-icons
;; ---------------------------------------
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
;; directory 우선/한글파일명 순 정렬불가(macOS) → 해결(setenv "LC_COLLATE" "C")
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

;; ======================================
;;; ace-window
;; --------------------------------------
;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (global-set-key (kbd "M-o") 'ace-window))

(global-set-key (kbd "M-o") 'other-window) ;ace-window 대체
;; ======================================
;;; magit
;; --------------------------------------
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
