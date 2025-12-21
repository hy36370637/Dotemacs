;;; my-search.el --- Web search and weather functions -*- lexical-binding: t; -*-

;; ======================================
;;; Requirements
;; ======================================
(require 'url)
(require 'dom)

;; ======================================
;;; Helper Functions
;; ======================================
(defun my--get-search-url (search-option query)
  "Generate search URL based on SEARCH-OPTION and QUERY."
  (let ((encoded-query (url-hexify-string query)))
    (cond
     ((string-equal search-option "Naver")
      (format "https://search.naver.com/search.naver?query=%s" encoded-query))
     ((string-equal search-option "Google")
      (format "https://www.google.com/search?q=%s" encoded-query))
     ((string-equal search-option "Namuwiki")
      (format "https://namu.wiki/w/%s" encoded-query))
     (t nil))))

(defun my--open-url (search-option url query)
  "Open URL based on SEARCH-OPTION with QUERY."
  (cond
   ((string-equal search-option "macOS Dictionary")
    (call-process "open" nil 0 nil (concat "dict://" (url-hexify-string query))))
   ((and url (not (string-empty-p url)))
    (browse-url url))
   (t
    (message "Invalid search option"))))

;; ======================================
;;; Text Search Function
;; ======================================
(defun my-search-text-in-range ()
  "Search selected text.
Search options: macOS Dictionary, Naver, Google, Namuwiki."
  (interactive)
  (let* ((query (if (use-region-p)
                    (buffer-substring-no-properties 
                     (region-beginning) (region-end))
                  ""))
         (search-option (completing-read "Choose option: " 
                                         '("macOS Dictionary" "Naver" 
                                           "Google" "Namuwiki"))))
    (if (string-empty-p query)
        (message "텍스트를 선택해주세요.")
      (let ((url (my--get-search-url search-option query)))
        (my--open-url search-option url query)))))

(defun my-search-content-in-dir ()
  "사용자가 선택한 디렉토리 내의 파일 내용을 consult-ripgrep으로 검색."
  (interactive)
  (let* ((default-dir "~/Dropbox/Docs/org/")
         (selected-dir (read-directory-name "검색할 디렉토리 선택: " default-dir default-dir t)))
    (consult-ripgrep selected-dir)))

;; ======================================
;;; Weather Search Functions
;; ======================================
(defun my--parse-weather-data (dom city)
  "Parse weather data from DOM for CITY and display in buffer."
  (let* ((temperature-elem (dom-by-class dom "temperature_text"))
         (temperature (when temperature-elem 
                        (string-replace " " "" 
                                        (dom-texts (car temperature-elem)))))
         (summary-elem (dom-by-class dom "summary"))
         (summary (when summary-elem 
                    (string-replace " " "" 
                                    (dom-texts (car summary-elem)))))
         (weather-elem (dom-by-class dom "weather before_slash"))
         (weather (when weather-elem 
                    (string-replace " " "" 
                                    (dom-texts (car weather-elem)))))
         (dust-elems (dom-by-class dom "today_chart_list"))
         (dust-info (when dust-elems
                      (cl-remove-duplicates
                       (mapcar (lambda (elem)
                                 (cons (dom-texts (dom-by-class elem "title"))
                                       (dom-texts (dom-by-class elem "txt"))))
                               dust-elems)
                       :test #'equal)))
         (sunset-elem (dom-by-class dom "item_today type_sun"))
         (sunset-info (when sunset-elem
                        (format "%s %s"
                                (dom-texts (dom-by-class sunset-elem "title"))
                                (dom-texts (dom-by-class sunset-elem "txt")))))
         (weekly-forecast (dom-by-class dom "week_item"))
         (weekly-info (mapcar 
                       (lambda (day)
                         (list (dom-texts (dom-by-class day "date"))
                               (dom-texts (dom-by-class day "weather"))
                               (string-replace "최저기온" "" 
                                               (dom-texts (dom-by-class day "lowest")))
                               (string-replace "최고기온" "" 
                                               (dom-texts (dom-by-class day "highest")))))
                       weekly-forecast)))
    
    ;; Display weather information
    (with-current-buffer (get-buffer-create (format "*%s 날씨*" city))
      (erase-buffer)
      
      ;; Current weather
      (insert (format "%s 날씨 정보\n -----\n온도: %s\n날씨: %s … %s\n"
                      city
                      (or temperature "정보 없음")
                      (or summary "정보 없음")
                      (or weather "정보 없음")))
      
      ;; Sunset/sunrise info
      (when sunset-info
        (insert (format "%s\n" sunset-info)))
      
      ;; Air quality info
      (dolist (dust dust-info)
        (insert (format "%s: %s\n" (car dust) (cdr dust))))
      
      ;; Weekly forecast
      (insert "\n주간 날씨 :\n")
      (dolist (day weekly-info)
        (insert (format "%s: %s 기온 %s/%s\n"
                        (nth 0 day) (nth 1 day) 
                        (nth 2 day) (nth 3 day))))
      
      ;; Setup buffer
      (goto-char (point-min))
      (local-set-key (kbd "q") 'quit-window)
      (pop-to-buffer (current-buffer)))))

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

(defun my-weather-search ()
  "Interactive weather search command."
  (interactive)
  (let ((city (read-string "도시명 입력: ")))
    (my-naver-weather-search city)))




(provide 'my-search)
;;; my-search.el ends here
