;; ======================================
;;; org
;; ======================================
;; org-mode를 사용한 일상기록 유지
;; /emacs/lisp/my-org-custom.el
;; Key bindings
(use-package org
  :bind
  (("M-n" . outline-next-visible-heading)
   ("M-p" . outline-previous-visible-heading)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-directory (expand-file-name "~/Docs/org/"))
  (setq org-agenda-files '("Tasks.org" "Daily.org"))
  ;;  (setq org-startup-indented nil)                 ;indent-mode enable
  ;; hard indent
  (setq org-adapt-indentation t)		;heading 이하 들여쓰기
  (setq org-hide-leading-stars t)
  (setq org-src-preserve-indentation t)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("x" . "example")
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
     "xelatex -interaction nonstopmode -output-directory %o %f"))
  )

;; ======================================
;;; org-agenda
;; ======================================
(setq org-agenda-prefix-format
      '((agenda . " %t %s")  ;" %t %-12:c%?-12t% s"
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
(setq org-agenda-format-date "%Y-%m-%d (%a)")  ; 날자 포맷. 가독성 높힘

  ;; ;; Agenda view customizations
  ;; (org-agenda-custom-commands
  ;;   '(("d" "Custom agenda view"
  ;;      ((agenda "" ((org-agenda-span 'week)
  ;;                   (org-agenda-start-on-weekday 0)
  ;;                   (org-agenda-format-date "%Y-%m-%d")))))))

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


(provide 'my-org-custom)
