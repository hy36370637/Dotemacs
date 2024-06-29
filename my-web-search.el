;;; -*- lexical-binding: t; -*-
;; =======================================
;;; Naver 날씨정보
;; ======================================-
(require 'url)
(require 'dom)

(defun naver-weather-search ()
  "도시명 입력 → 네이버 날씨 정보 표시."
  (interactive)
  (let* ((city (read-string "도시명 입력: "))
         (encoded-city (url-hexify-string city)))
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
                    (dust-elem (dom-by-class dom "item_today"))
                    (dust-info (when dust-elem
                                 (split-string (dom-texts (car dust-elem)) "\n" t " ")))
                    (weekly-forecast (dom-by-class dom "week_item"))
                    (weekly-info (mapcar (lambda (day)
                                           (list (dom-texts (dom-by-class day "date"))
                                                 (dom-texts (dom-by-class day "weather"))
                                                 (dom-texts (dom-by-class day "temperature_low"))
                                                 (dom-texts (dom-by-class day "temperature_high"))))
                                         weekly-forecast)))
               (with-current-buffer (get-buffer-create (format "*%s 날씨*" city))
                 (erase-buffer)
                 (insert (format "%s 날씨 정보:\n\n온도: %s\n날씨: %s\n미세먼지: %s\n초미세먼지: %s\n\n"
                                 city
                                 (or temperature "정보 없음")
                                 (or summary "정보 없음")
                                 (or (nth 0 dust-info) "정보 없음")
                                 (or (nth 1 dust-info) "정보 없음")))
                 (insert "주간 날씨:\n")
                 (dolist (day weekly-info)
                   (insert (format "%s: %s, 기온 %s/%s\n"
                                   (nth 0 day) (nth 1 day) (nth 2 day) (nth 3 day))))
		 (goto-char (point-min)) ; 버퍼의 첫 부분으로 이동
                 (local-set-key (kbd "q") 'quit-window)
                 (pop-to-buffer (current-buffer))))
           (error
            (message "날씨 데이터 파싱 중 오류 발생: %s" err)))))
     (list city)  ; city 변수를 람다 함수의 인자로 전달
     t)))

;; ======================================
;;; 선택한 범위 문자열 → Web 검색(Naver, Google)
;; ======================================
(defun my/region-search-web (start end)
  "Search selected text on Naver or Google."
  (interactive "r")
  (let* ((query (buffer-substring-no-properties start end))
         (search-engine (completing-read "Choose search engine: " '("Naver" "Google")))
         (url (cond ((string-equal search-engine "Naver")
                     (concat "https://search.naver.com/search.naver?query="
                             (url-hexify-string query)))
                    ((string-equal search-engine "Google")
                     (concat "https://www.google.com/search?q="
                             (url-hexify-string query))))))
    (browse-url url)))




;; end here
(provide 'my-web-search)
