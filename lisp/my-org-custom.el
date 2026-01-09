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

(defun my-org-insert-image ()
  "Insert and display image"
  (interactive)
  (let* ((img-base-dir (expand-file-name "img/" org-directory))
         (selected-file (read-file-name "이미지 선택: " img-base-dir nil t)))
    (when (and selected-file (not (file-directory-p selected-file)))
      (insert (format "[[file:%s]]\n" selected-file))
      (org-display-inline-images))))

(defun my-insert-image-path ()
  "Insert relative image path"
  (interactive)
  (let* ((base-dir (expand-file-name "img/" org-directory))
         (selected-file (read-file-name "img 선택: " base-dir nil t))
         (relative-path (when (and selected-file (file-exists-p selected-file))
                          (file-relative-name selected-file))))
    (if relative-path
        (progn
          (insert (concat "./" relative-path))
          (message "완료: %s" relative-path))
      (message "선택 취소되었습니다."))))

(defvar my/pngpaste-bin 
  (or (executable-find "pngpaste") "/opt/homebrew/bin/pngpaste")
  "pngpaste executable path.")
(defun my-org-screenshot (chdir name)
  "Insert screenshot from clipboard"
  (interactive 
   (let* ((default-dir (file-name-concat org-directory "img/"))
          (chosen-dir (read-directory-name "저장 폴더: " default-dir default-dir t))
          (default-name (format-time-string "%Y%m%d_%H%M%S")) ;; 현재 날짜_시간 생성
          (file-name (read-string (format "파일 이름 입력(기본값 %s, 확장자 제외): " default-name) 
                                  nil nil default-name)))
     (list chosen-dir file-name)))  
  (let* ((pngpaste-bin (or (executable-find "pngpaste") "/opt/homebrew/bin/pngpaste"))
         (path (expand-file-name (concat name ".png") chdir)))
    (make-directory chdir t)
    (if (zerop (shell-command (format "%s %s" pngpaste-bin (shell-quote-argument path))))
        (progn
          (insert (format "\n#+ATTR_LATEX: :width 0.5\\textwidth\n#+CAPTION: %s\n[[file:%s]]\n" 
                          name path))
          (org-display-inline-images)
          (message "이미지 저장 성공: %s" path))
      (error "클립보드 이미지 없음 or pngpaste 실행 실패"))))

;; (defun my-org-generate-toc ()
;;   "Auto-generate table of contents(PDF Export 제외)."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     ;; 기존 목차 삭제
;;     (when (re-search-forward "^\\* 목차.*: noexport:" nil t)
;;       (org-cut-subtree))
;;     ;; 새 목차 생성
;;     (goto-char (point-min))
;;     (when (re-search-forward "^\\* " nil t)
;;       (beginning-of-line)
;;       (insert "* 목차 : noexport:\n")
;;       (let ((toc-items '()))
;;         (org-map-entries
;;          (lambda ()
;;            (let* ((level (org-current-level))
;;                   (title (org-get-heading t t t t)))
;;              (when (> level 1)
;;                (push (format "%s- [[*%s][%s]]"
;;                              (make-string (* 2 (1- level)) ?\s)
;;                              title title)
;;                      toc-items))))
;;          nil 'file)
;;         (insert (mapconcat #'identity (reverse toc-items) "\n"))
;;         (insert "\n\n"))))
;;   (message "목차 생성 완료"))

;; (defun my-set-latex-cover-image ()
;;   "표지 이미지를 선택하고 LaTeX title-command를 설정합니다. 이미지 너비를 지정할 수 있습니다."
;;   (interactive)
;;   (let* ((cover-img-dir (expand-file-name "img/imgCover/" org-directory))
;;          (selected-file (read-file-name "표지 이미지 선택: " cover-img-dir nil t))
;;          ;; 너비를 입력받되, 기본값으로 14.7cm를 제시합니다.
;;          (img-width (read-string "이미지 너비 (기본 14.7cm): " nil nil "14.7cm")))
;;     (when (and selected-file (not (file-directory-p selected-file)))
;;       ;; 버퍼 로컬 변수로 설정 (현재 파일에만 적용)
;;       (make-variable-buffer-local 'org-latex-title-command)
      
;;       ;; \vspace를 제거하고 \vfill을 사용하여 상하 균형을 맞춥니다.
;;       (setq org-latex-title-command
;;             (format "\\begin{titlepage}\n\\centering\n\\vfill\n\\includegraphics[width=%s]{%s}\n\\vfill\n\\end{titlepage}\n"
;;                     img-width
;;                     (file-relative-name selected-file)))
;;       (message "LaTeX 표지 설정 완료: %s (너비: %s)" 
;;                (file-name-nondirectory selected-file) 
;;                img-width))))

(defun cal-fixLayout () 
  "Fix calendar layout for monospace Korean font."
  (face-remap-add-relative 'default 
                           '(:family "Noto Sans Mono CJK KR" :height 160)))

;; ======================================
;;; Calendar
;; ======================================
(use-package calendar
  :ensure nil
  :hook (calendar-mode . cal-fixLayout)
  :custom
  (calendar-week-start-day 0)  ; Start week on Sunday
  (calendar-date-style 'iso)   ; YYYY-MM-DD 형식
  (calendar-month-name-array 
   ["1월" "2월" "3월" "4월" "5월" "6월" 
    "7월" "8월" "9월" "10월" "11월" "12월"]))

;; ======================================
;;; org-mode
;; ======================================
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-directory (expand-file-name "~/Dropbox/Docs/org"))
  (org-startup-indented t)             ;시작때 indent mode enable
  (org-startup-with-inline-images nil)
  (org-startup-folded t)
  (org-adapt-indentation nil)          ;indent의 실제 공백 nil 
  (org-indent-indentation-per-level 2)
  (org-edit-src-content-indentation 0)
  (org-image-actual-width 400)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO" "HOLD" "DONE")))
  (org-structure-template-alist
          '(("c" . "center")
            ("C" . "comment")
            ("e" . "src emacs-lisp")
	    ("s" . "src")
            ("q" . "quote")
	    ("v" . "verse")
	    ("x" . "example")))
  (org-export-with-drawers nil)
  (org-agenda-format-date "%Y-%m-%d (%a)")
  (org-agenda-current-time-string "← now ─────────")
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  :config
;;  (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  (setq org-agenda-files
        (seq-filter #'file-exists-p
                    (list (my-org-person-file-path "Holidays.org")
                          (my-org-person-file-path "Tasks.org")
                          (my-org-person-file-path "Daily.org"))))
  (setq org-capture-templates
        `(("d" "Daily" entry
           (file+datetree ,(my-org-person-file-path "Daily.org"))
           "* %?"
           :empty-lines-after 1)
          
          ("t" "Tasks" entry
           (file ,(my-org-person-file-path "Tasks.org"))
           "* TODO %?\nSCHEDULED: %t"
           :empty-lines-after 1)
          
          ("r" "Reading" entry
           (file ,(my-org-person-file-path "cReading.org"))
           "* %?\n기록일: %U"
           :empty-lines-after 1)

	  ("m" "경조사" table-line
	   (file ,(my-org-person-file-path "aMoney.org"))
	   ,(concat "| %^{구분} | %^{일자|" (format-time-string "%Y.%m.%d") "} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |")
           :prepend nil))))

(defun my-capture-cReading-access ()
  "Access cReading.org right away"
  (interactive)
  (require 'org)
  (require 'org-capture)
  (let ((org-capture-templates
         `(("r" "Reading" entry
            (file ,(my-org-person-file-path "cReading.org"))
            "* %^{제목}\n%^{내용}\n기록일: %U"
            :empty-lines-after 1
            :immediate-finish t)))
        (org-capture-after-finalize-hook
         (list (lambda ()
                 (when (get-buffer "*Org Select*")
                   (kill-buffer "*Org Select*"))
                 (when (get-buffer "CAPTURE-cReading.org")
                   (kill-buffer "CAPTURE-cReading.org"))))))
    (org-capture nil "r")))

;; ======================================
;;; org-superstar
;; ======================================
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; org-bullets-bullet-list 대신 아래 변수를 사용합니다.
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "▶" "▷" "►")))

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
   '("latexmk -pdflatex='xelatex -shell-escape -interaction=nonstopmode' -pdf -f %f")))

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
