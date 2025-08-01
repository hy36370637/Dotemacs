;;; -*- lexical-binding: t; -*-
;; ======================================
;;; org
;; ======================================
;; /.emacs.d/lisp/my-org-custom.el

(defun my-org-person-file-path (filename)
  "Construct the full path for a personal org file."
  (expand-file-name filename my/org-person-dir))

(use-package org
  :ensure nil				;built-in
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-directory (expand-file-name "~/Dropbox/Docs/org/"))
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-startup-with-inline-images nil)
  (org-support-shift-select t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-image-actual-width 400)
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE"))))

;; (use-package ox-md
;;   :after org   ; org 모드가 로드된 후에 ox-md를 로드
;;   :config
;;   (require 'ox-md)
;;   (add-to-list 'org-export-backends 'md))

(use-package ox-latex
  :ensure nil  ; built in
 :after org
 :commands (org-latex-export-to-latex org-latex-export-to-pdf)
  :custom
  (org-latex-title-command "\\maketitle \\newpage")
  (org-latex-toc-command "\\tableofcontents \\newpage")
  (org-latex-compiler "xelatex")
  (org-latex-to-pdf-process
   '("xelatex -interaction=nonstopmode -synctex=1 %f"
     "xelatex  -interaction=nonstopmode -synctex=1 %f"))
  :config
  (add-to-list 'org-export-backends 'latex))

;; ======================================
;;; org-capture
;; ======================================
(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
   (setq org-capture-templates
	 `(("d" "Daily" entry (file+datetree ,(my-org-person-file-path "Daily.org")) "* %?")
           ("t" "Tasks" entry (file ,(my-org-person-file-path "Tasks.org")) "* TODO %?")
           ("r" "Read" entry (file ,(my-org-person-file-path "cReading.org")) "* %?")
	   ;; ("D" "동강fNote" entry (file+datetree ,(my-org-person-file-path "DG_fNote.org")) "* %?")
           ;; ("G" "금산fNote" entry (file+datetree ,(my-org-person-file-path "GS_fNote.org")) "* %?")
	   ("m" "경조사" table-line (file, (my-org-person-file-path "aMoney.org")) "| %^{구분} | %^{일자} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |" :prepend nil))))

;; ======================================
;;; org-agenda
;; ======================================
(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files (list (my-org-person-file-path "Tasks.org")
                               (my-org-person-file-path "Daily.org")))
  (setq org-agenda-format-date "%Y-%m-%d (%a)")       ; 날자 포맷. 가독성
  (setq org-agenda-current-time-string "← now ───────")
  (setq org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))  ;완료항목 hidden
  (setq org-agenda-include-diary t))              	;holidays 포함

;; ======================================
;;; org-bullets
;; ======================================
(use-package org-bullets
  :ensure nil
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "●" "◇" "◆" "▷" "▶")))

;; ======================================
;;; holidays/calendar
;; ======================================
(defun my/setup-calendar-holidays ()
  "Set up custom holidays and solar terms for the calendar."
  (setq my-holidays
        '((holiday-fixed 1 1 "새해")
;;          (holiday-chinese  1  1 "설날")
          (holiday-fixed 1 29 "딸日")
;;          (holiday-fixed 3 1 "삼일절")
          (holiday-fixed 3 19 "결혼일")
;;          (holiday-chinese  4  8 "석탄일")
;;          (holiday-fixed 5 5 "어린이날")
;;          (holiday-fixed 6 6 "현충일")
          (holiday-fixed 6 10 "아들日")
;;          (holiday-fixed 7 17 "제헌절")
          (holiday-chinese  7  21 "장인제")
;;          (holiday-chinese  8  4 "조부제")
;;          (holiday-fixed 8 15 "광복절")
;;          (holiday-chinese  8 15 "추석")
          (holiday-chinese  9  3 "母生")
          (holiday-chinese  9  5 "父祭")
;;          (holiday-fixed 10 3 "개천절")
;;          (holiday-fixed 10 9 "한글날")
          (holiday-chinese  11  9 "장모생신")
          (holiday-fixed 12 25 "성탄절")))
  ;; 24절기 설정
  (setq 24solar-holidays
        '((holiday-fixed 2 4 "입춘")
          (holiday-fixed 2 19 "우수")
          (holiday-fixed 3 5 "경칩")
          (holiday-fixed 3 20 "춘분")
          (holiday-fixed 4 5 "청명")
          (holiday-fixed 4 20 "곡우")
          (holiday-fixed 5 5 "입하")
          (holiday-fixed 5 21 "소만")
          (holiday-fixed 6 6 "망종")
          (holiday-fixed 6 21 "하지")
          (holiday-fixed 7 7 "소서")
          (holiday-fixed 7 22 "대서")
          (holiday-fixed 8 7 "입추")
          (holiday-fixed 8 23 "처서")
          (holiday-fixed 9 7 "백로")
          (holiday-fixed 9 22 "추분")
          (holiday-fixed 10 8 "한로")
          (holiday-fixed 10 23 "상강")
          (holiday-fixed 11 7 "입동")
          (holiday-fixed 11 22 "소설")
          (holiday-fixed 12 7 "대설")
          (holiday-fixed 12 22 "동지")
          (holiday-fixed 1 5 "소한")
          (holiday-fixed 1 20 "대한")))
  ;; 휴일 설정 초기화 및 적용
  (setq holiday-general-holidays nil
        holiday-local-holidays nil
        holiday-other-holidays nil
        holiday-christian-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        calendar-mark-holidays-flag t)
  (setq calendar-holidays (append my-holidays 24solar-holidays)))

(defun cal-fixLayout ()
  "calendar layout for D2Coding font. 고정폭 깨짐방지"
  (face-remap-add-relative 'default '(:family "Noto Sans Mono CJK KR" :height 160)))

(use-package calendar
  :ensure nil
  :hook ((calendar-mode . cal-fixLayout)
         ((calendar-mode org-agenda-mode) . my/setup-calendar-holidays))
  :config
  (setq calendar-week-start-day 0	;일요일부터 시작
	calendar-day-header-array ["일" "월" "화" "수" "목" "금" "토"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"])
  (custom-set-faces
   '(holiday ((t (:foreground "red" :weight bold))))))

;; ======================================
;;; for org edit/custom function
;; --------------------------------------
;; (with-eval-after-load 'org  ; in org-mode
;;   (define-key org-mode-map (kbd "C-0") (lambda () 
;;                                          (interactive)
;; 					 (end-of-line)
;;                                          (newline-and-indent)
;;                                          (next-line)
;;                                          (org-cycle)))
;;   (define-key org-mode-map (kbd "C-8") (lambda () 
;;                                          (interactive)
;; 					 (end-of-line)
;;                                          (newline-and-indent)))
;;   (define-key org-mode-map (kbd "C-9") (lambda () 
;;                                          (interactive)
;; 					 (end-of-line)
;;                                          (org-insert-heading))))

;; ======================================
;;; denote
;; ======================================
;; Denote 설정
(use-package denote
  :ensure nil
  :bind("C-c SPC n" . denote)
  :custom
  (denote-directory (concat org-directory "denote"))
  (denote-known-keywords '("work" "personal" "reading"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; 기본값으로 org 파일 사용
  (denote-prompts '(title keywords))
  :config
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New  Denote" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))
;;   (consult-denote-default-file-type 'org))



;;; end here
(provide 'my-org-custom)
