;;; -*- lexical-binding: t; -*-
;; ======================================
;;; org
;; ======================================
;; /.emacs.d/lisp/my-org-custom.el

(defun my-org-person-file-path (filename)
  "Construct the full path for a personal org file."
  (expand-file-name filename my/org-person-dir))

(use-package org
  :ensure nil
  :defer t
  :commands (org-mode org-agenda org-capture org-version)
  :mode ("\\.org\\'" . org-mode)
  ;; 전역 단축키 설정 - :bind 섹션에서
  :bind (("C-c a" . org-agenda)
         ("C-c C" . org-capture))
  :custom
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
  :config
  ;; org-agenda 설정
  (setq org-agenda-files (list (my-org-person-file-path "Anniversary.org")
			       (my-org-person-file-path "Tasks.org") 
                               (my-org-person-file-path "Daily.org")))
  (setq org-agenda-format-date "%Y-%m-%d (%a)")
  (setq org-agenda-current-time-string "← now ─────────")
  (setq org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
  (setq org-agenda-include-diary t)
  
  ;; org-capture 설정
  (setq org-capture-templates
        `(("d" "Daily" entry (file+datetree ,(my-org-person-file-path "Daily.org")) "* %?")
          ("t" "Tasks" entry (file ,(my-org-person-file-path "Tasks.org")) "* TODO %?")
          ("r" "Read" entry (file ,(my-org-person-file-path "cReading.org")) "* %?")
          ("m" "경조사" table-line (file ,(my-org-person-file-path "aMoney.org")) 
           "| %^{구분} | %^{일자} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |" :prepend nil))))

;; ======================================
;;; ox-latex
;; ======================================
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
;;; org-bullets
;; ======================================
(use-package org-bullets
  :ensure nil
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "◆" "▷" "▶")))

;; ======================================
;;; org-md
;; ======================================
(use-package ox-md
  :after org)

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



;;; end here
(provide 'my-org-custom)
