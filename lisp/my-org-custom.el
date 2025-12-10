;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-org-custom.el
;; ======================================
;;; Helper Functions
;; ======================================
(defun my-org-person-file-path (filename)
  "Construct the full path for a personal org file."
  (expand-file-name filename my/org-person-dir))

;; ======================================
;;; org
;; ======================================
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c a" . org-agenda)
   ("C-c C" . org-capture))
  :custom
  ;; 기본 설정
  (org-directory (expand-file-name "~/Dropbox/Docs/org/"))
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-startup-with-inline-images nil)
  (org-startup-folded t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-image-actual-width 400)
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
  (org-export-with-drawers nil)
  ;; Agenda 설정
  (org-agenda-format-date "%Y-%m-%d (%a)")
  (org-agenda-current-time-string "← now ─────────")
  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
  (org-agenda-include-diary t)
  :config
  ;; Agenda 파일 목록
  (setq org-agenda-files
        (list (my-org-person-file-path "Anniversary.org")
              (my-org-person-file-path "Tasks.org")
              (my-org-person-file-path "Daily.org")))
  
  ;; Capture 템플릿
  (setq org-capture-templates
        `(("d" "Daily" entry
           (file+datetree ,(my-org-person-file-path "Daily.org"))
           "* %?")
          ("t" "Tasks" entry
           (file ,(my-org-person-file-path "Tasks.org"))
           "* TODO %?")
          ("r" "Read" entry
           (file ,(my-org-person-file-path "cReading.org"))
           "* %?")
          ("m" "경조사" table-line
           (file ,(my-org-person-file-path "aMoney.org"))
           "| %^{구분} | %^{일자} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |"
           :prepend nil))))

;; ======================================
;;; ox-latex
;; ======================================
(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-title-command "\\maketitle \\newpage")
  (org-latex-toc-command "\\tableofcontents \\newpage")
  (org-latex-compiler "xelatex")
  (org-latex-to-pdf-process
   '("xelatex -interaction=nonstopmode -synctex=1 %f"
     "xelatex -interaction=nonstopmode -synctex=1 %f"))
  :config
  (add-to-list 'org-export-backends 'latex))

;; ======================================
;;; org-bullets
;; ======================================
(use-package org-bullets
  :ensure nil
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "◎" "●" "○" "◆" "▷" "▶")))

;; ======================================
;;; ox-md
;; ======================================
(use-package ox-md
  :ensure nil   ;built in
  :after org)

;;; end here
(provide 'my-org-custom)
