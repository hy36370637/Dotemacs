;;; -*- lexical-binding: t; -*-
;; ======================================
;;; 검색: 선택한 문자 or 사용자 입력 도시명
;; ======================================
(require 'url)
(require 'dom)
(defun my/search-selected-text ()
  "Search selected text in macOS Dictionary, Naver, Google, or Namuwiki.
If the selected text includes '날씨', perform a weather search using my/naver-weather-search."
  (interactive)
  (let* ((query (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  ""))
         (search-option (completing-read "Choose option: " 
                                         '("macOS Dictionary" "Naver" "Google" "Namuwiki" "날씨 검색")))
         (url (cond 
               ((string-equal search-option "macOS Dictionary")
                (concat "dict://" (url-hexify-string query)))
               ((string-equal search-option "Naver")
                (concat "https://search.naver.com/search.naver?query="
                        (url-hexify-string query)))
               ((string-equal search-option "Google")
                (concat "https://www.google.com/search?q="
                        (url-hexify-string query)))
               ((string-equal search-option "Namuwiki")
                (concat "https://namu.wiki/w/" (url-hexify-string query))))))
    (cond
     ;; 날씨 검색일 경우, 도시명을 입력받아 검색 수행
     ((string-equal search-option "날씨 검색")
      (let ((city (read-string "도시명을 입력하세요: ")))
        (my/naver-weather-search city)))
     
     ;; 다른 옵션을 선택했는데 선택된 텍스트가 없는 경우
     ((and (not (string-equal search-option "날씨 검색")) (string-empty-p query))
      (message "텍스트를 선택해주세요."))
     
     ;; 선택된 텍스트가 있는 경우, URL을 열어줍니다.
     (t
      (if (string-equal search-option "macOS Dictionary")
          (call-process "open" nil 0 nil url)
        (browse-url url))))))


(defun my/naver-weather-search (city)
  "Search Naver weather information for the given CITY."
  (let* ((encoded-city (url-hexify-string city)))
    (url-retrieve
     (format "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%s%%20날씨" encoded-city)
     (lambda (status city)
       (if (or (not status) (plist-get status :error))
           (message "날씨 정보를 가져오는 중 오류 발생: %s" (plist-get status :error))
         (condition-case err
             (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                    (temperature-elem (dom-by-class dom "temperature_text"))
                    (temperature (when temperature-elem 
                                   (replace-regexp-in-string "[ \n]" "" (dom-texts (car temperature-elem)))))
                    (summary-elem (dom-by-class dom "summary"))
                    (summary (when summary-elem
                               (replace-regexp-in-string "[ \n]" "" (dom-texts (car summary-elem)))))
                    (weather-elem (dom-by-class dom "weather before_slash"))
                    (weather (when weather-elem
                               (replace-regexp-in-string "[ \n]" "" (dom-texts (car weather-elem)))))
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
                    (weekly-info (mapcar (lambda (day)
                                           (list (dom-texts (dom-by-class day "date"))
                                                 (dom-texts (dom-by-class day "weather"))
                                                 (replace-regexp-in-string "최저기온" "" (dom-texts (dom-by-class day "lowest")))
                                                 (replace-regexp-in-string "최고기온" "" (dom-texts (dom-by-class day "highest")))))
                                         weekly-forecast)))
               (with-current-buffer (get-buffer-create (format "*%s 날씨*" city))
                 (erase-buffer)
                 (insert (format "%s 날씨 정보\n -----\n온도: %s\n날씨: %s … %s\n"
                                 city
                                 (or temperature "정보 없음")
                                 (or summary "정보 없음")
                                 (or weather "정보 없음")))
                 (when sunset-info
                   (insert (format "%s\n" sunset-info)))
                 (dolist (dust dust-info)
                   (insert (format "%s: %s\n" (car dust) (cdr dust))))
                 (insert "\n주간 날씨 :\n")
                 (dolist (day weekly-info)
                   (insert (format "%s: %s 기온 %s/%s\n"
                                   (nth 0 day) (nth 1 day) (nth 2 day) (nth 3 day))))
                 (goto-char (point-min))
                 (local-set-key (kbd "q") 'quit-window)
                 (pop-to-buffer (current-buffer))))
           (error
            (message "날씨 데이터 파싱 중 오류 발생: %s" err)))))
     (list city)
     t)))

;; ======================================
;;; 골프 명언
;; ======================================
;; (defun show-random-golf-quote ()
;;   "Display a random 골프 명언"
;;   (interactive)
;;   (let* ((file-path "~/.emacs.d/lisp/gQuote.txt") ;골프 명언 파일
;;          (quotes (with-temp-buffer
;;                    (insert-file-contents file-path)
;;                    (split-string (buffer-string) "\n" t)))
;;          (random-quote (nth (random (length quotes)) quotes)))
;;     (setq frame-title-format random-quote))) ;display title bar 
;; ;;    (message "%s" random-quote))) ; display minibuffer
;; ;; (add-hook 'auto-save-hook 'show-random-golf-quote)
;;



;; end here
(provide 'my-web-search)
