;;; my-org-custom.el --- Optimized Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Org-mode configuration with optimized performance and structure

;;; Code:

;; ======================================
;;; Variables
;; ======================================
(defvar my/org-person-dir (expand-file-name "~/Dropbox/Docs/Person/")
  "Directory for personal org files.")

(defvar my/pngpaste-bin 
  (or (executable-find "pngpaste") "/opt/homebrew/bin/pngpaste")
  "pngpaste executable path.")

;; ======================================
;;; Helper Functions
;; ======================================
(defun my-org-person-file-path (filename)
  "Construct the full path for a personal org file FILENAME."
  (expand-file-name filename my/org-person-dir))

;;; ###autoload
(defun my-org-insert-image ()
  "Insert and display image"
  (interactive)
  (let* ((img-base-dir (expand-file-name "img/" org-directory))
         (selected-file (read-file-name "Select image: " img-base-dir nil t)))
    (when (and selected-file (not (file-directory-p selected-file)))
      (insert (format "[[file:%s]]\n" selected-file))
      (org-display-inline-images))))

;;; ###autoload
(defun my-insert-image-path ()
  "Insert relative image path"
  (interactive)
  (let* ((base-dir (expand-file-name "img/" org-directory))
         (selected-file (read-file-name "Select image: " base-dir nil t))
         (relative-path (when (and selected-file (file-exists-p selected-file))
                          (file-relative-name selected-file))))
    (if relative-path
        (progn
          (insert (concat "./" relative-path))
          (message "완료: %s" relative-path))
      (message "선택 취소되었습니다."))))

;;; ###autoload
(defun my-org-screenshot (chdir name)
  "Insert a screenshot from the clipboard into the current Org buffer.
   Requires: brew install pngpaste"
  (interactive 
   (let* ((default-dir (file-name-concat org-directory "img/"))
          ;; Prompt user for the target directory
          (chosen-dir (read-directory-name "Target directory: " default-dir default-dir t))
          ;; Generate default filename based on current timestamp
          (default-name (format-time-string "%Y%m%d_%H%M%S"))
          ;; Prompt user for filename (defaults to timestamp)
          (file-name (read-string (format "Enter filename (default %s, exclude extension): " default-name) 
                                  nil nil default-name)))
     (list chosen-dir file-name)))  
  (let* ((pngpaste-bin (or (executable-find "pngpaste") "/opt/homebrew/bin/pngpaste"))
         (path (expand-file-name (concat name ".png") chdir)))
    ;; Ensure the target directory exists
    (make-directory chdir t)
    ;; Attempt to paste from clipboard using pngpaste
    (if (zerop (shell-command (format "%s %s" pngpaste-bin (shell-quote-argument path))))
        (progn
          ;; Insert Org-mode syntax with LaTeX attributes and a caption
          (insert (format "\n#+ATTR_LATEX: :width 0.5\\textwidth\n#+CAPTION: %s\n[[file:%s]]\n" 
                          name path))
          ;; Refresh inline images display
          (org-display-inline-images)
          (message "Image saved successfully: %s" path))
      (error "No image in clipboard or pngpaste execution failed"))))

;;; ###autoload
(defun my-org-insert-drawer-custom (&optional arg drawer)
  "Prompt the user to select and insert a drawer from an expanded list.
Includes system-reserved drawers (PROPERTIES, LOGBOOK, RESULTS) and 
common user-defined drawers (MEMO, NOTE, DETAIL).
If ARG is non-nil, insert at the end of the current outline node."
  (interactive "P")
  (let* ((choices '("PROPERTIES" "LOGBOOK" "MEMO" "NOTE" "CONTEXT" "DETAIL" "SOLUTION"))
         (name (or drawer
                   (completing-read "Drawer name (Select or Type): " 
                                    choices nil nil))))
    (org-insert-drawer arg name)))

(defun my-org-daily-info()
  "Generate lunar date and tide information string for org-capture."
  (let* ((lunar-str (my-lunar-date-string))
         (lunar-cleaned (string-trim (replace-regexp-in-string "(음) " "" lunar-str)))
         (tide-result (my-format-tide-info))
         (tide-times (car tide-result))
         (muldae (string-trim (cdr tide-result))))
    (format "\n- 음력: %s | 물때: %s\n%s"
            lunar-cleaned
            (if (string-match "(\\(.*\\))" muldae)
                (match-string 1 muldae)
              muldae)
            tide-times)))


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

;; (defun cal-fixLayout () 
;;   "Fix calendar layout"
;;   (face-remap-add-relative 'default 
;;                            '(:family "Noto Sans Mono CJK KR" :height 160)))

;; ======================================
;;; Calendar
;; ======================================
(use-package calendar
  :ensure nil
;; :hook (calendar-mode . cal-fixLayout)
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
  :hook (org-mode . (lambda () (text-scale-increase 1)))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
	 ;; ("C-c C" . my-org-popup-capture)
         :map org-mode-map
         ("C-c C-x d" . my-org-insert-drawer-custom)
	 ("C-," . my-pair-pairs-wrap)
	 ("M-," . org-insert-structure-template))
  :custom
  ;; (org-directory (expand-file-name "~/Dropbox/Docs/org")) ; -> init.el
  (org-startup-indented t)             ;시작때 indent mode enable
  (org-startup-with-inline-images nil)
  (org-startup-folded t)
  (org-startup-with-drawer t)          ;파일을 열 때 Drawer를 자동으로 접음
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
           ;; "* %?"                          ;; 음력제외
	   "* %?\n%(my-org-daily-info)" 
           :empty-lines-after 1)
          
          ("t" "Tasks" entry
           (file ,(my-org-person-file-path "Tasks.org"))
           "* TODO %?\nSCHEDULED: %t"
           :empty-lines-after 1)
          
          ("r" "Reading" entry
           (file ,(my-org-person-file-path "cReading.org"))
           "* %?\n기록일: %U"
   	   :unnarrowed t
           :empty-lines-after 1)

	  ("m" "경조사" table-line
	   (file ,(my-org-person-file-path "aMoney.org"))
	   ,(concat "| %^{구분} | %^{일자|" (format-time-string "%Y.%m.%d") "} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |")
           :prepend nil))))


;; ======================================
;;; org-superstar
;; ======================================
(use-package org-superstar
  :ensure nil
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "▶" "▷" "►")))

;; ======================================
;;; ox-appear
;; ======================================
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t     ;; 먼저 선언 필수
	org-appear-autoemphasis t
	org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.2))

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
;; (use-package ox-md
;;   :ensure nil
;;   :after org)

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

;; ======================================
;;; denote
;; ======================================
(use-package denote
  :bind
  (("C-c n n" . denote)                       ; 새 노트 생성
   ;; ("C-c n N" . my-window-popup-denote)       ; Pop-up
   ("C-c n i" . denote-link)                  ; 현재 노트에 다른 노트 링크 삽입
   ("C-c n b" . denote-show-backlinks-buffer) ; 현재 노트를 참조하는 다른 노트들 보기
   ("C-c n r" . denote-rename-file))          ; 기존 파일 이름을 denote 형식으로 변경
  :config
  (setq denote-directory (expand-file-name "denote" org-directory))
  (setq denote-file-type nil)
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory t)))

;; ======================================
;;; View-mode Custom
;; ======================================
;; Enable read-only protection when entering view-mode
(setq view-read-only t) 

(defun my-view-mode-edit-instantly ()
  "Disable view-mode immediately and switch to edit mode."
  (interactive)
  (when view-mode
    (view-mode -1)
    (message "Switched to Edit Mode")))

;; View-mode Configuration
(with-eval-after-load 'view
  ;; Assign 'e' key for instant transition to editing
  (define-key view-mode-map (kbd "e") 'my-view-mode-edit-instantly))

;; Visual enhancements when toggling view-mode
(add-hook 'view-mode-hook
          (lambda ()
            (if view-mode
                (progn
                  (hl-line-mode 1)               ; Enable line highlighting
                  (setq-local cursor-type 'bar))  ; Change cursor to a bar for reading
	      	  ;; (set-face-background 'hl-line (face-background color-lighten)))
              (hl-line-mode -1)                 ; Disable line highlighting
              (setq-local cursor-type 'box))))   ; Restore box cursor for editing




(provide 'my-org-custom)
;;; my-org-custom.el ends here
