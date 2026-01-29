;;; my-search.el --- Web search and weather functions -*- lexical-binding: t; -*-

;; ======================================
;;; Requirements
;; ======================================
(require 'url)
(require 'dom)
(require 'cl-lib)
(require 'consult)

;; ======================================
;;; Configuration
;; ======================================
(defvar my-search-engines
  '(("macOS Dictionary" . dict)
    ("Naver" .  "https://search.naver.com/search.naver?query=%s")
    ("Naver Hanja" . "https://hanja.dict.naver.com/#/search?range=all&query=%s")
    ("Google" . "https://www.google.com/search?q=%s")
    ("Namuwiki" .  "https://namu.wiki/w/%s"))
  "Each element is of the form (NAME . URL/TYPE).")

(defvar my-docs-root "~/Dropbox/Docs/" "Root directory for documentation files.")

(defvar my-search-path-targets
  '(("Docs (All)"    . "~/Dropbox/Docs/")
    ("Org Files"     . "~/Dropbox/Docs/org/")
    ("PDF Files"     . "~/Dropbox/Docs/pdf/") 
    ("Notes/Person"  . "~/Dropbox/Docs/Person/")
    ("Denote"        . "~/Dropbox/Docs/org/denote/")
    ("Emacs Config"  . "~/.emacs.d/"))
  "List of main directories for ripgrep search.")

;; ======================================
;;; Helper Functions
;; ======================================
(defun my--get-search-url (engine query)
  "Generate a search URL from a search engine and a query."
  (let ((config (assoc engine my-search-engines)))
    (when config
      (let ((url-template (cdr config)))
        (if (eq url-template 'dict)
            (concat "dict://" (url-hexify-string query))
          (format url-template (url-hexify-string query)))))))

(defun my--open-url (url)
  "Open URL in the default web browser."
  (if (string-prefix-p "dict://" url)
      (call-process "open" nil 0 nil url)
    (browse-url url)))

(defun my--ripgrep-in-dir (dir)
  "Run consult-ripgrep in DIR."
  (let ((default-directory (expand-file-name dir)))
    (consult-ripgrep default-directory)))


(defun my-consult-ripgrep-pdf-grouped ()
  ;;brew install ripgrep-all
  "Search PDFs with file grouping - file -> line, C-g in line goes back"
  (interactive)
  (let* ((default-directory (cdr (assoc "PDF Files" my-search-path-targets)))
         (search-term (read-string "Search PDFs: "))
         (results (split-string
                   (shell-command-to-string
                    (format "rga -l %s ."
                            (shell-quote-argument search-term)))
                   "\n" t)))
    (catch 'exit
      (while t
        ;; ---------- 1단계 ----------
        (let ((selected-file
               (condition-case nil
                   (completing-read
                    (format "Files containing '%s': " search-term)
                    results nil t)
                 (quit (throw 'exit nil)))))
          ;; ---------- 2단계 ----------
          (condition-case nil
              (let* ((full-path (expand-file-name selected-file default-directory))
                     (lines (split-string
                             (shell-command-to-string
                              (format "rga -n %s %s"
                                      (shell-quote-argument search-term)
                                      (shell-quote-argument full-path)))
                             "\n" t))
                     (selected-line
                      (completing-read
                       (format "Results in %s (C-g=back): "
                               (file-name-nondirectory selected-file))
                       lines nil t)))
                ;; 페이지 번호 추출 시도 (예: "Page 5:" 형식)
                (let ((page-num
                       (when (string-match "Page \\([0-9]+\\)" selected-line)
                         (string-to-number (match-string 1 selected-line)))))
                  (if page-num
                      ;; Skim으로 특정 페이지 열기
                      (do-applescript
                       (format "tell application \"Skim\"
    activate
    open POSIX file \"%s\"
    tell front document to go to page %d
end tell" full-path page-num))
                    ;; 페이지 번호 없으면 기본 앱으로
                    (call-process "open" nil 0 nil full-path)))
                (throw 'exit nil))
            (quit
             (message "Back to file list"))))))))

;; (defun my-consult-ripgrep-pdf-grouped ()
;;   ;;brew install ripgrep-all
;;   "Search PDFs with file grouping - file -> line, C-g in line goes back"
;;   (interactive)
;;   (let* ((default-directory (cdr (assoc "PDF Files" my-search-path-targets)))
;;          (search-term (read-string "Search PDFs: "))
;;          (results (split-string
;;                    (shell-command-to-string
;;                     (format "rga -l %s ."
;;                             (shell-quote-argument search-term)))
;;                    "\n" t)))
;;     (catch 'exit
;;       (while t
;;         ;; ---------- 1단계 ----------
;;         (let ((selected-file
;;                (condition-case nil
;;                    (completing-read
;;                     (format "Files containing '%s': " search-term)
;;                     results nil t)
;;                  (quit (throw 'exit nil)))))  ;; ← 전체 종료

;;           ;; ---------- 2단계 ----------
;;           (condition-case nil
;;               (let* ((full-path (expand-file-name selected-file default-directory))
;;                      (lines (split-string
;;                              (shell-command-to-string
;;                               (format "rga -n %s %s"
;;                                       (shell-quote-argument search-term)
;;                                       (shell-quote-argument full-path)))
;;                              "\n" t))
;;                      (selected-line
;;                       (completing-read
;;                        (format "Results in %s (C-g=back): "
;;                                (file-name-nondirectory selected-file))
;;                        lines nil t)))
;;                 ;; 성공 → 파일 열고 종료
;;                 (call-process "open" nil 0 nil full-path)
;;                 (throw 'exit nil))
;;             (quit
;;              ;; 2단계 C-g → 아무것도 안 하고 while 계속
;;              (message "Back to file list"))))))))



;; ======================================
;;; Main Function
;; ======================================
(defun my-search-unified ()
  "Search content in a selected directory from `my-search-path-targets`."
  (interactive)
  (let* ((choice (completing-read "Search Content in: " my-search-path-targets))
         (path (cdr (assoc choice my-search-path-targets))))
    (if (string-match-p "PDF" choice)
	(my-consult-ripgrep-pdf-grouped)
      (my--ripgrep-in-dir path))))

;; ======================================
;;; Weather Search Functions
;; ======================================
;;; ###autoload
(defun my--parse-weather-data (dom city)
  "Extract weather data from Naver's DOM and visualize it in a dedicated buffer."
  (let* ((buffer-name (format "*날씨: %s*" city))
         ;; 1. 현재 온도 및 요약 정보 추출
         (temp-elem (car (dom-by-class dom "temperature_text")))
         (temperature (if temp-elem (string-trim (dom-texts temp-elem)) "정보 없음"))
         (summary-elem (car (dom-by-class dom "summary")))
         (summary (if summary-elem (string-trim (dom-texts summary-elem)) ""))
         (weather-elem (car (dom-by-class dom "weather before_slash")))
         (weather (if weather-elem (string-trim (dom-texts weather-elem)) ""))

         ;; 2. 미세먼지 등 상세 지표 (오늘의 차트)
         (dust-info (mapcar (lambda (elem)
                              (cons (string-trim (dom-texts (car (dom-by-class elem "title"))))
                                    (string-trim (dom-texts (car (dom-by-class elem "txt"))))))
                            (dom-by-class dom "today_chart_list")))

         ;; 3. 일몰/일출 정보
         (sunset-elem (car (dom-by-class dom "item_today type_sun")))
         (sunset-info (when sunset-elem
                        (format "%s: %s"
                                (dom-texts (car (dom-by-class sunset-elem "title")))
                                (dom-texts (car (dom-by-class sunset-elem "txt"))))))

         ;; 4. 주간 예보 파싱
         (weekly-info (mapcar 
                       (lambda (day)
                         (list (string-trim (dom-texts (car (dom-by-class day "date"))))
                               (string-trim (dom-texts (car (dom-by-class day "weather"))))
                               (string-trim (string-replace "최저기온" "" 
                                                            (dom-texts (car (dom-by-class day "lowest")))))
                               (string-trim (string-replace "최고기온" "" 
                                                            (dom-texts (car (dom-by-class day "highest")))))))
                       (dom-by-class dom "week_item"))))

    ;; 버퍼 생성 및 데이터 삽입
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; 헤더 출력
        (insert (propertize (format " @ %s 날씨 정보\n" city) 'face 'bold-italic))
        (insert (make-string 30 ?-) "\n")
        
        ;; 현재 상태
        (insert (propertize "현재 온도: " 'face 'bold) temperature "\n")
        (insert (propertize "날씨 요약: " 'face 'italic) summary " / " weather "\n")
        
        (when sunset-info
          (insert sunset-info "\n"))
        
        (insert "\n[대기 환경]\n")
        (dolist (dust (cl-remove-duplicates dust-info :test #'equal))
          (insert (format "- %-8s: %s\n" (car dust) (cdr dust))))
        
        ;; 주간 예보 (표 형식 비슷하게 정렬)
        (insert "\n[주간 예보]\n")
        (dolist (day weekly-info)
          (cl-destructuring-bind (date sky low high) day
            (insert (format "%-6s | %-6s | %3s / %3s\n" 
                            date sky low high))))
        
        ;; 버퍼 설정
        (goto-char (point-min))
        (special-mode) ; q 키로 닫기, 읽기 전용 등 기본 설정 적용
        (setq-local truncate-lines t))
      (pop-to-buffer (current-buffer)))))

;;; ###autoload
(defun my-naver-weather-search (city)
  "Search Naver weather information for CITY."
  (let ((encoded-city (url-hexify-string city)))
    (url-retrieve
     (format "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%s%%20날씨" 
             encoded-city)
     (lambda (status city)
       (if (plist-get status :error)
           (message "날씨 정보를 가져오는 중 오류 발생: %s" 
                    (plist-get status :error))
         (condition-case err
             (let ((dom (libxml-parse-html-region (point-min) (point-max))))
               (my--parse-weather-data dom city))
           (error
            (message "날씨 데이터 파싱 중 오류 발생: %s" err)))))
     (list city)
     t)))

(defun my-search-weather ()
  "Interactive weather search command."
  (interactive)
  (let ((city (read-string "도시명 입력: ")))
    (my-naver-weather-search city)))

;; ======================================
;;; Sync embark
;; ======================================
(with-eval-after-load 'embark
  (defun my-embark-web-search (query)
    "Execute web search for QUERY via Embark."
    (interactive "sSearch query: ")
    (let* ((engine (completing-read "Search engine: " (mapcar #'car my-search-engines)))
           (url (my--get-search-url engine query)))
      (if (and url (not (string-empty-p url)))
          (my--open-url url)
        (message "Invalid search query or URL."))))

  (let ((target-maps (list embark-identifier-map embark-region-map)))
    (dolist (map target-maps)
      (define-key map (kbd "S") #'my-embark-web-search)))

  (add-to-list 'embark-keymap-alist '(my-search)))




(provide 'my-search)
;;; my-search.el ends here
