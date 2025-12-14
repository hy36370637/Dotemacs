;;; my-org-custom.el --- Optimized Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Org-mode configuration with optimized performance and structure

;;; Code:

;; ======================================
;;; Variables
;; ======================================
(defvar my/org-person-dir (expand-file-name "~/Dropbox/Docs/Person/")
  "Directory for personal org files.")

;; ======================================
;;; Helper Functions
;; ======================================
(defun my-org-person-file-path (filename)
  "Construct the full path for a personal org file FILENAME."
  (expand-file-name filename my/org-person-dir))

;; ======================================
;;; org-mode
;; ======================================
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c C" . org-capture))
  :custom
  (org-directory (expand-file-name "~/Dropbox/Docs/org"))
  (org-startup-indented t)
  (org-startup-with-inline-images nil)
  (org-startup-folded t)
  (org-adapt-indentation nil)
  (org-indent-indentation-per-level 2)
  (org-image-actual-width 400)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
  (org-export-with-drawers nil)
  (org-agenda-format-date "%Y-%m-%d (%a)")
  (org-agenda-current-time-string "← now ─────────")
  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
  :config
  (setq org-agenda-files
        (seq-filter #'file-exists-p
                    (list (my-org-person-file-path "Holidays.org")
                          (my-org-person-file-path "Tasks.org")
                          (my-org-person-file-path "Daily.org"))))
  (setq org-capture-templates
        `(("d" "Daily" entry
           (file+datetree ,(my-org-person-file-path "Daily.org"))
           "* %?"
	   :empty-lines 1
           :unnarrowed t)
          
          ("t" "Tasks" entry
           (file ,(my-org-person-file-path "Tasks.org"))
           "* TODO %?\nSCHEDULED: %t"
	   :empty-lines 1
           :unnarrowed t)
          
          ("r" "Reading" entry
           (file ,(my-org-person-file-path "cReading.org"))
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
	   :empty-lines 1
           :unnarrowed t)
          
          ("m" "경조사" table-line
           (file ,(my-org-person-file-path "aMoney.org"))
           "| %^{구분} | %^{일자}U | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |"
           :prepend nil
           :table-line-pos "II-1"))))

;; ======================================
;;; org-bullets
;; ======================================
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "▶" "▷" "►")))

;; ======================================
;;; ox-latex
;; ======================================
(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-title-command "\\maketitle\\newpage")
  (org-latex-toc-command "\\tableofcontents\\newpage")
  (org-latex-pdf-process
   '("xelatex -interaction=nonstopmode -shell-escape %f"
     "xelatex -interaction=nonstopmode -shell-escape %f")))

;; ======================================
;;; ox-md
;; ======================================
(use-package ox-md
  :ensure nil
  :after org)

;; ======================================
;;; Performance Optimizations
;; ======================================
(with-eval-after-load 'org
  ;; Agenda 성능 향상
  (setq org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
        org-agenda-dim-blocked-tasks nil)
  
  ;; Fontification 성능 개선
  (setq org-fontify-whole-heading-line nil
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))
  

(provide 'my-org-custom)
;;; my-org-custom.el ends here
