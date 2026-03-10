;;; my-org-custom.el --- Optimized Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Org-mode configuration with centralized file paths and health tracking.

;;; Code:

;; ======================================
;;; 1. Variables & File Paths
;; ======================================
(defvar my/org-person-dir (expand-file-name "~/Dropbox/Docs/Person/")
  "Directory for personal org files.")

(defvar my/f-daily  (expand-file-name "Daily.org"  my/org-person-dir))
(defvar my/f-tasks  (expand-file-name "Tasks.org"  my/org-person-dir))
(defvar my/f-health (expand-file-name "Health.org" my/org-person-dir))
(defvar my/f-read   (expand-file-name "cReading.org" my/org-person-dir))
(defvar my/f-money  (expand-file-name "aMoney.org"  my/org-person-dir))

(defvar my/pngpaste-bin 
  (or (executable-find "pngpaste") "/opt/homebrew/bin/pngpaste")
  "pngpaste executable path.")


;; ======================================
;;; 2. Helper Functions
;; ======================================
;; (defun my-org-person-file-path (filename)
;;   "Construct the full path for a personal org file FILENAME."
;;   (expand-file-name filename my/org-person-dir))

;;; ###autoload
(defun my-org-insert-image ()
  "Insert an image. Select [inline] or [path] with arrow keys."
  (interactive)
  (let* ((choice (completing-read "Insert type: " '("inline" "path") nil t))
         (base-dir (expand-file-name "img/" org-directory))
         (file (read-file-name "Select image: " base-dir nil t)))
    (when (and file (not (file-directory-p file)))
      (cond
       ((equal choice "inline")
        (insert (format "[[file:%s]]\n" file))
        (org-display-inline-images))
       ((equal choice "path")
        (insert (concat "./" (file-relative-name file))))
       (t (message "Cancelled."))))))


;;; ###autoload
(defun my-org-screenshot (chdir name)
  "Insert a screenshot from the clipboard into the current Org buffer.
   Requires: brew install pngpaste"
  (interactive 
   (let* ((default-dir (file-name-concat org-directory "img/"))
          (chosen-dir (read-directory-name "Target directory: " default-dir default-dir t))
          (default-name (format-time-string "%Y%m%d_%H%M%S"))
          (file-name (read-string (format "Enter filename (default %s, exclude extension): " default-name) 
                                  nil nil default-name)))
     (list chosen-dir file-name)))  
  (let* ((path (expand-file-name (concat name ".png") chdir)))
    (make-directory chdir t)
    (if (zerop (shell-command (format "%s %s" my/pngpaste-bin (shell-quote-argument path))))
        (progn
          (insert (format "\n#+ATTR_LATEX: :width 0.5\\textwidth\n#+CAPTION: %s\n[[file:%s]]\n" 
                          name path))
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


;;; ###autoload
(defun my-paste-with-parentheses ()
  "Insert the clipboard content enclosed in parentheses ()."
  (interactive)
  (let ((text (gui-get-selection 'CLIPBOARD 'STRING)))
    (unless text (setq text (current-kill 0)))
    (if (and text (not (string-empty-p text)))
        (insert (format "(%s)" text))
      (message "Clipboard is empty."))))


(defun my/org-latex-filter-blocks (text backend info)
  "Apply global style to quote/verse blocks based on :quote-style option."
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((style (plist-get info :quote-style))
           (style-alist
            '(("1" . "{\\small\n%s}")
              ("2" . "\\begin{tcolorbox}[colback=gray!10, boxrule=0.5pt, arc=0pt]\\small\n%s\\end{tcolorbox}")
              ("3" . "\\begin{tcolorbox}[colback=gray!10, boxrule=0.5pt, arc=0pt]\n%s\\end{tcolorbox}")
              ("4" . "\\begin{tcolorbox}[colback=gray!10, boxrule=0pt, arc=0pt]\\small\n%s\\end{tcolorbox}")
              ("5" . "\\begin{tcolorbox}[colback=gray!10, boxrule=0pt, arc=0pt]\n%s\\end{tcolorbox}")))
           (template (cdr (assoc style style-alist))))
      (if template (format template text) text))))


;; ======================================
;;; 3. Health & Blood Pressure Logic
;; ======================================
(defun my/get-bp-stats ()
  "Reads the overall average blood pressure data from the Health.org table."
  (let ((file my/f-health))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward "^|\\s-*\\[" nil t)
           (goto-char (match-beginning 0))
           (let* ((table (org-table-to-lisp))
                  (data (seq-filter (lambda (x)
                                      (and (listp x)
                                           (not (string-match "일시\\|평균\\|--" (car x)))))
                                    table))
                  (sys-list (mapcar (lambda (row) (string-to-number (string-trim (nth 1 row)))) data))
                  (dia-list (mapcar (lambda (row) (string-to-number (string-trim (nth 2 row)))) data))
                  (pul-list (mapcar (lambda (row) (string-to-number (string-trim (nth 3 row)))) data))
                  (n (float (length data))))
             (if (> n 0)
                 (list (/ (apply '+ sys-list) n) 
                       (/ (apply '+ dia-list) n) 
                       (/ (apply '+ pul-list) n) 
                       (length data))
               nil))))))))


(defvar my/bp-start-date (encode-time 0 0 0 4 3 2026)
  "BP💊 start date.")

(defun my/Bdays ()
  "Returns a string in the format 'BP💊 nD: [TimeOfDay]/'."
  (let* ((diff-days (1+ (floor (/ (float-time 
                                   (time-subtract (current-time) my/bp-start-date)) 
                                  86400))))
         (hour (string-to-number (format-time-string "%H")))
         (time-tag (cond ((< hour 6)  "새벽") ((< hour 12) "오전") ((< hour 14) "점심")
                         ((< hour 18) "오후") ((< hour 20) "저녁") (t "밤"))))
    (format "BP💊 %dD: %s/" diff-days time-tag)))


(defun my/get-recent-bp-stats (days-offset &optional period)
  "Calculates blood pressure averages for a specific period (default: 7 days)."
  (let* ((period (or period 7))
         (file my/f-health)
         (now (current-time))
         (end-date (time-subtract now (days-to-time days-offset)))
         (start-date (time-subtract end-date (days-to-time period)))
         (sys-list nil))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward "^|\\s-*\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
           (let* ((row-date (date-to-time (match-string 1)))
                  (cols (org-split-string (thing-at-point 'line) "|"))
                  (sys-str (and (> (length cols) 1) (nth 1 cols)))
                  (sys-val (when sys-str (string-to-number (string-trim sys-str)))))
             (when (and sys-val (> sys-val 0)
                        (time-less-p start-date row-date)
                        (time-less-p row-date end-date))
               (push sys-val sys-list)))))))
    (if sys-list (/ (apply '+ sys-list) (float (length sys-list))) nil)))


(defun my-bp-report ()
  "Displays a weekly blood pressure report in the echo area."
  (interactive)
  (let* ((this-week (my/get-recent-bp-stats 0))
         (last-week (my/get-recent-bp-stats 7))
         (diff (when (and this-week last-week) (- this-week last-week))))
    (cond
     (this-week
      (message "📎주간 BP 리포트: 이번주 %.1f %s"
               this-week
               (if last-week
                   (format "(지난주 %.1f 대비 %+.1f %s)" 
                           last-week diff (if (<= diff 0) "▼ 개선!" "▲ 주의"))
                 "(지난주 데이터 없음)")))
     (t (message "No BP data found.")))))


(defun my/org-capture-finish-bp-report ()
  "Run BP report if the capture key is 'b'."
  (when (string= (org-capture-get :key) "b")
    (run-with-timer 0.5 nil #'my-bp-report)))


(defun my/org-capture-finalize-blood-pressure-habit ()
  "혈압(b) 캡처가 끝나면 '혈압 측정' Habit 항목을 찾아 완료 처리합니다."
  (when (equal (org-capture-get :key) "b")
    (save-excursion
      ;; my/f-health 변수에 담긴 파일 경로를 사용합니다.
      (let ((path (expand-file-name my/f-health)))
        (with-current-buffer (find-file-noselect path)
          (goto-char (point-min))
          ;; 'TODO 혈압 측정' 항목을 정확히 찾습니다.
          (when (re-search-forward "^\\*+ TODO 혈압 측정" nil t)
            (org-todo "DONE")
            (save-buffer)))))))

(add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-finalize-blood-pressure-habit)


(defun my-show-bp-stats-by-tag ()
  "데이터 무결성 검토 완료: 시간대별 정렬 및 하단 통합 통계 리포트."
  (interactive)
  (let ((stats-hash (make-hash-table :test 'equal))
        (path (expand-file-name my/f-health))
        (time-order '("새벽" "오전" "점심" "오후" "저녁" "밤"))
        (total-sys 0.0) (total-dia 0.0) (total-pul 0.0) (total-count 0)
        stats-list)
    (with-current-buffer (find-file-noselect path)
      (save-excursion
        (goto-char (point-min))
        ;; 정규식 검토: 수축기(1), 이완기(2), 맥박(3), 태그(4)
        (while (re-search-forward "| \\[[^]]+\\] | +\\([0-9.]+\\) | +\\([0-9.]+\\) | +\\([0-9.]+\\) | \\(?:BP💊 [0-9]+D\\|BF\\): +\\([^ \t\n|]+\\)" nil t)
          (let* ((sys (string-to-number (match-string 1)))
                 (dia (string-to-number (match-string 2)))
                 (pul (string-to-number (match-string 3)))
                 (tag (match-string 4))
                 (current-data (gethash tag stats-hash (list 0.0 0.0 0.0 0))))
            ;; 태그별 누적 (실수 연산 보장)
            (puthash tag 
                     (list (+ (nth 0 current-data) sys)
                           (+ (nth 1 current-data) dia)
                           (+ (nth 2 current-data) pul)
                           (1+ (nth 3 current-data)))
                     stats-hash)
            ;; 전체 누적
            (setq total-sys (+ total-sys sys)
                  total-dia (+ total-dia dia)
                  total-pul (+ total-pul pul)
                  total-count (1+ total-count))))))
    
    ;; 정렬을 위해 리스트 변환
    (maphash (lambda (k v) (push (cons k v) stats-list)) stats-hash)
    
    ;; 정렬: 사용자 정의 시간 순서 적용
    (setq stats-list
          (sort stats-list
                (lambda (a b)
                  (let* ((tag-a (car a))
                         (tag-b (car b))
                         (prefix-a (car (split-string tag-a "/")))
                         (prefix-b (car (split-string tag-b "/")))
                         (idx-a (cl-position prefix-a time-order :test 'equal))
                         (idx-b (cl-position prefix-b time-order :test 'equal)))
                    (if (and idx-a idx-b (not (= idx-a idx-b)))
                        (< idx-a idx-b)
                      (string< tag-a tag-b))))))

    ;; 결과 출력 버퍼 구성
    (with-current-buffer (get-buffer-create "*Blood Pressure Stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== 통합 혈압 분석 리포트 (BF & BP💊) ===\n")
        (insert (format "분석 일시: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "%-22s | %-8s | %-8s | %-5s\n" "상태 (시간대/상태)" "수축기" "이완기" "횟수"))
        (insert "--------------------------------------------------------------\n")
        (dolist (entry stats-list)
          (let* ((tag (car entry))
                 (data (cdr entry))
                 (count (nth 3 data)))
            (insert (format "%-22s | %-8.1f | %-8.1f | %-5d\n"
                            tag 
                            (/ (nth 0 data) (float count))
                            (/ (nth 1 data) (float count))
                            count))))
        
        ;; 하단 전체 평균 라인 (요청하신 형식)
        (when (> total-count 0)
          (insert "--------------------------------------------------------------\n")
          (insert (format "전체 평균               | %-8.1f | %-8.1f | %-8.1f (총횟수 %d)\n"
                          (/ total-sys total-count)
                          (/ total-dia total-count)
                          (/ total-pul total-count)
                          total-count)))
        
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))


;; ======================================
;;; 4. Main Org Configuration
;; ======================================
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . (lambda () (text-scale-increase 1)))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-M-y" . my-paste-with-parentheses)
         ("M-,"   . org-insert-structure-template)
         ("C-c C-x d" . my-org-insert-drawer-custom))
  :custom
  (org-agenda-files (list my/f-tasks my/f-daily my/f-health))
  (org-startup-indented t)
  ;; (org-startup-with-inline-images nil)   ;default
  (org-startup-folded t)
  (org-adapt-indentation nil)               ;indent의 실제 공백 nil 
  ;; (org-indent-indentation-per-level 2)   ;default
  (org-edit-src-content-indentation 0)
  (org-image-actual-width 400)
  (org-startup-with-drawer t)
  (org-log-into-drawer t)                   ;상태 변경 기록 :LOGBOOK: 서랍 안
  (org-log-repeat 'time)
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
  ;; (org-export-with-drawers nil)           ; default
  (org-export-with-smart-quotes t)           ; ""
  (org-export-with-special-strings t)        ; - -- ---
  (org-export-with-sub-superscripts '{})     ; _
  (org-agenda-format-date "%Y-%m-%d (%a)")
  (org-agenda-current-time-string "← now ─────────")
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-inhibit-startup t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  ;; (org-agenda-dim-blocked-tasks nil)      ;default
  ;; (org-fontify-whole-heading-line nil)    ;default
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-habit-preceding-days 7)
  (org-habit-following-days 1)
  (org-habit-show-habits-only-for-today t)   ;today일 경우만 아젠다 뷰 보임

  :config
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-capture-after-finalize-hook #'my/org-capture-finish-bp-report)
  
  (setq org-capture-templates
        (let ((today (format-time-string "%Y-%m-%d")))
          `( ;; "d" Daily
            ("d" "Daily" entry (file+datetree ,my/f-daily)
             "* %?\n기록일: %U" :empty-lines-after 1)
            
            ;; "t" Tasks
            ("t" "Tasks" entry (file ,my/f-tasks)
             "* TODO %?\nSCHEDULED: %t" :empty-lines-after 1)
      
            ("b" "Blood Pressure" table-line (file+headline ,my/f-health "혈압 데이터")
             "| %U | %^{수축기} | %^{이완기} | %^{맥박} | %(my/Bdays)%^{상태|일반|기상직후|복용전|식후|운동후} %(let ((s (my/get-bp-stats))) (if s (format \" (Avg:%d)\" (truncate (car s))) \"\")) %^{메모} |" 
             :prepend t :immediate-finish t)

            ;; "h" Habit
            ("h" "Habit: 혈압" entry (file+headline ,my/f-health "습관 관리")
             "* TODO 혈압 측정\nSCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:END:" :immediate-finish t)

            ;; "r" Reading
            ("r" "Reading" entry (file ,my/f-read)
             "* %?\n기록일: %U" :unnarrowed t :empty-lines-after 1)

            ;; "m" 경조사
            ("m" "경조사" table-line (file ,my/f-money)
             ,(concat "| %^{구분} | %^{일자|" today "} | %^{이름} | %^{연락처} | %^{관계} | %^{종류} | %^{금액} | %^{메모} |")
             :prepend nil)))))


;; ======================================
;;; 5. External Packages
;; ======================================
(use-package org-superstar
  :ensure nil
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "▶" "▷" "►")))


(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.2))


(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-title-command "\\maketitle\\newpage")
  (org-latex-toc-command "\\tableofcontents\\newpage")
  (org-latex-pdf-process
   '("latexmk -pdflatex='xelatex -shell-escape -interaction=nonstopmode' -pdf -f %f"))
  :config
  ;; Export filter
  (add-to-list 'org-export-options-alist '(:quote-style "QUOTE_STYLE" nil nil t))
  (add-to-list 'org-export-filter-quote-block-functions #'my/org-latex-filter-blocks)
  (add-to-list 'org-export-filter-verse-block-functions #'my/org-latex-filter-blocks))
  

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 0)  ; Start week on Sunday
  (calendar-date-style 'iso)   ; YYYY-MM-DD 형식
  (calendar-month-name-array 
   ["1월" "2월" "3월" "4월" "5월" "6월" 
    "7월" "8월" "9월" "10월" "11월" "12월"]))


(use-package valign
  :hook (org-mode . valign-mode))


;; ======================================
;;; denote
;; ======================================
(use-package denote
  :bind
  (("C-c n n" . denote)                       ; 새 노트 생성
   ("C-c n i" . denote-link)                  ; 현재 노트에 다른 노트 링크 삽입
   ("C-c n b" . denote-show-backlinks-buffer) ; 현재 노트를 참조하는 다른 노트들 보기
   ("C-c n r" . denote-rename-file))          ; 기존 파일 이름을 denote 형식으로 변경
  :config
  (setq denote-directory (expand-file-name "denote" org-directory))
  (setq denote-file-type nil)
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory t)))


(provide 'my-org-custom)
;;; my-org-custom.el ends here
