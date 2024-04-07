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
  :custom
  (org-hide-leading-stars nil)
  (org-startup-with-inline-images nil)
  (org-src-preserve-indentation t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-image-actual-width '(100))
  ;; Org directory and agenda files
  ;;  (org-directory (expand-file-name (if my-laptop-p "~/Docs/org/" "~/Dropbox/Docs/org/")))
  (org-directory (expand-file-name "~/Docs/org/"))
  (org-agenda-files '("Tasks.org" "Daily.org"))
  ;; Todo keywords
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
  ;; Capture templates
  (org-capture-templates
    '(("d" "Daily" entry (file+datetree "Daily.org") "* %?")
      ("t" "Tasks" entry (file+olp "Tasks.org" "Schedule") "* TODO %?")
      ("a" "Assist" table-line (file+headline "eMoney.org" "eMoney")
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
  :hook (org-mode . org-indent-mode)  ; auto indent
  )
  ;; ;; Agenda view customizations
  ;; (org-agenda-custom-commands
  ;;   '(("d" "Custom agenda view"
  ;;      ((agenda "" ((org-agenda-span 'week)
  ;;                   (org-agenda-start-on-weekday 0)
  ;;                   (org-agenda-format-date "%Y-%m-%d")))))))
;;
;; ======================================
;;; org-bullets
;; ======================================
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "●" "○" "●")))
;;
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
