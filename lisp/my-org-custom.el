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
;;  (org-support-shift-select t)
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
;; (use-package denote
;;   :ensure nil
;; ;;  :bind("C-c SPC n" . denote)
;;   :custom
;;   (denote-directory (concat org-directory "denote"))
;;   (denote-known-keywords '("work" "personal" "reading"))
;;   (denote-infer-keywords t)
;;   (denote-sort-keywords t)
;;   (denote-file-type nil) ; 기본값으로 org 파일 사용
;;   (denote-prompts '(title keywords))
;;   :config
;;   (with-eval-after-load 'org-capture
;;     (add-to-list 'org-capture-templates
;;                  '("n" "New  Denote" plain
;;                    (file denote-last-path)
;;                    #'denote-org-capture
;;                    :no-save t
;;                    :immediate-finish nil
;;                    :kill-buffer t
;;                    :jump-to-captured t))))
;; ;;   (consult-denote-default-file-type 'org))



;;; end here
(provide 'my-org-custom)
