;;; -*- lexical-binding: t; -*-
;; ======================================
;;; org
;; ======================================
;; /emacs/lisp/my-org-custom.el
(use-package org
  :ensure nil
  :bind
  (("M-n" . outline-next-visible-heading)
   ("M-p" . outline-previous-visible-heading)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture))
  :config
  (setq org-directory (expand-file-name "~/Docs/org/"))
  ;;  (setq org-startup-indented nil)                 ;indent-mode enable
  ;; hard indent
  (setq org-adapt-indentation t)		;heading 이하 들여쓰기
  (setq org-hide-leading-stars t)
  (setq org-src-preserve-indentation t)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("v" . "verse")
          ("q" . "quote")))
  :custom
  (org-startup-with-inline-images nil)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-image-actual-width '(100))
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
  (org-capture-templates
   '(("d" "Daily" entry (file+datetree "Daily.org") "* %?")
     ("t" "Tasks" entry (file+olp "Tasks.org" "Schedule") "* TODO %?")
     ("a" "Assist" table-line (file+headline "aMoney.org" "aMoney")
      "| %^{구분} | %^{일자} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |")
     ("f" "FarmNote" entry (file+datetree "dFarmNote.org") "* %?")))
  ;; Export settings
  (org-latex-title-command "\\maketitle \\newpage")
  (org-latex-toc-command "\\tableofcontents \\newpage")
  (org-latex-compiler "xelatex")
  (org-latex-to-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f")))

;; ======================================
;;; org-agenda
;; ======================================
(use-package org
  :ensure nil
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files '("Tasks.org" "Daily.org"))
  (setq org-agenda-prefix-format
	'((agenda . " %t %s")  ;" %t %-12:c%?-12t% s"
          (timeline . "  % s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-format-date "%Y-%m-%d (%a)")  ; 날자 포맷. 가독성 높힘
  (setq org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) ;완료항목 hidden
  (setq org-agenda-include-diary t))	;holidays 포함

;; ======================================
;;; org-bullets
;; ======================================
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "●" "○" "●")))

;; ======================================
;;; for org edit/custom function
;; --------------------------------------
(defun org-custom-action (at)
  "Perform custom org-mode action based on the numeric ACTION.
   8: new line, 9: new org-heading, 0: paragraph & org-cycle"
  (interactive "nEnter action (8: new line, 9: heading, 0: new paragraph): ")
  (end-of-line)
  (cond
   ((= at 8) (newline-and-indent))
   ((= at 9) (org-insert-heading))
   ((= at 0) (progn (newline-and-indent) (next-line) (org-cycle)))
   (t (message "err,, please enter 8, 9, or 0."))))

(global-set-key (kbd "C-0") 'org-custom-action)

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
;;; my-latex-custom-function/for org-mode
;; --------------------------------------
;; org-mode export → LaTeX, PDF
(defun latex-text-color (text color)
  "Return LaTeX text with specified color."
  (format "\\textcolor{%s}{%s}" color text))

(defun insert-latex-text-color (begin end)
  "latex. selected-text color change"
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties begin end))
            (color (read-string "Enter color: ")))
        (delete-region begin end)
        (insert (latex-text-color selected-text color)))
    (message "No region selected")))

(defun latex-modify-text (begin end modifier)
  "Modify selected text, '_ for subscript and '^ for superscript."
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties begin end)))
        (delete-region begin end)
        (setq selected-text (concat modifier "{" selected-text "}"))
        (insert selected-text))
    (message "No region selected")))

;; 단락으로 분명하게 구분된 영역만 사용
;; C-c C-, org-insert-structure-template 사용 권장
;; (defun my-latex-insert-block () ;; ver 0.2.1
;;   "Inserts `#+begin_block`, `#+end_block`. selected region."
;;   (let ((block-type (completing-read "Choose block type: " '("quote" "verse"))))
;;     (if (use-region-p)
;;         (let ((beg (region-beginning))
;;               (end (region-end))
;;               (indent ""))
;;           ;; Determine the current indentation level
;;           (save-excursion
;;             (goto-char beg)
;;             (skip-chars-forward "[:space:]")
;;             (setq indent (concat (make-string (current-column) ?\s))))
;;           (save-excursion
;;             (goto-char beg)
;;             (insert (format "%s#+begin_%s\n" indent block-type))
;;             ;; Apply the same indentation to the end of the block
;;             (goto-char end)
;;             (end-of-line)
;;             (insert-before-markers (format "\n%s#+end_%s" indent block-type))))
;;       (message "No region selected"))))

;;; 통합본
(defun my-org-latex-custom(begin end)
  "for Org export LaTeX. Text color, sub-Super Script, quote-verse Block"
  (interactive "r")
  (if (use-region-p)
      (let ((choice (read-char-choice "Select action: [c]글자색, [s]아래첨자, [S]위첨자: " '(?c ?s ?S))))
        (pcase choice
          (?c (insert-latex-text-color begin end))
          (?s (latex-modify-text begin end "_"))
          (?S (latex-modify-text begin end "^")))
    (message "No region selected"))))
(global-set-key (kbd "C-9") 'my-org-latex-custom)





;;; end here
(provide 'my-org-custom)