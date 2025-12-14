;;; my-todays-pop.el --- Today's information display -*- lexical-binding: t; -*-
;; Author: Ho Young <hy36370637@gmail.com>

;;; Commentary:
;; Display today's date, agenda, and random quotes from cReading.org
;; Holidays are now integrated via org-agenda (no separate holiday lookup needed)

;;; Code:

;; ======================================
;;; Global Dependencies
;; ======================================
(require 'url)
(require 'xml)
(require 'json)

;; ======================================
;;; KASI API Settings and Helper
;; ======================================
;; API
(defvar kasi-api-key nil
  "KASI API")

(defun kasi-load-api-key ()
  (let ((key-file (expand-file-name "lunar_api" 
                                    (file-name-directory 
                                     (or load-file-name buffer-file-name)))))
    (if (file-exists-p key-file)
        (with-temp-buffer
          (insert-file-contents key-file)
          (setq kasi-api-key (string-trim (buffer-string))))
      (error "lunar_api 파일을 찾을 수 없습니다: %s" key-file))))

;; 초기화 시 API 로드
(unless kasi-api-key
  (kasi-load-api-key))

(defun kasi-get-xml-text (item-node tag)
  "주어진 XML 노드(item-node)에서 특정 태그(tag)의 텍스트 콘텐츠 추출."
  (when item-node
    (car (xml-node-children (car (xml-get-children item-node tag))))))

(defun kasi-get-lunar-date (year month day)
  (unless kasi-api-key
    (kasi-load-api-key))
  (let* ((url (format "http://apis.data.go.kr/B090041/openapi/service/LrsrCldInfoService/getLunCalInfo?solYear=%s&solMonth=%s&solDay=%s&ServiceKey=%s"
                      year month day kasi-api-key))
          (buffer (url-retrieve-synchronously url))
          lunar-data)
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (let* ((xml-data (xml-parse-region (point) (point-max)))
               (response (car xml-data))
               (body (car (xml-get-children response 'body)))
               (items (car (xml-get-children body 'items)))
               (item (car (xml-get-children items 'item))))
           
          (when item
            (setq lunar-data
                  (list
                    :year (kasi-get-xml-text item 'lunYear)
                    :month (kasi-get-xml-text item 'lunMonth)
                    :day (kasi-get-xml-text item 'lunDay)
                    :leap (kasi-get-xml-text item 'lunLeapmonth))))))
      (kill-buffer buffer))
    lunar-data))

(defun my-calculate-muldae (lunar-day)
  "음력 날짜로 물때 계산(남해안 녹동항 기준 - 8물때식)."
  (let* ((day (string-to-number lunar-day))
         (cycle-pos (cond
                      ((= day 1) 8) ((= day 2) 9) ((= day 3) 10) ((= day 4) 11)
                      ((= day 5) 12) ((= day 6) 13) ((= day 7) 14)
                      ((= day 8) 0) ; 조금
                      ((<= 9 day 15) (- day 8))
                      ((= day 16) 8) ((= day 17) 9) ((= day 18) 10) ((= day 19) 11)
                      ((= day 20) 12) ((= day 21) 13) ((= day 22) 14)
                      ((= day 23) 0) ; 조금
                      ((<= 24 day 30) (- day 23))
                      (t 1))))
    cycle-pos))

(defun my-lunar-date-string ()
  "오늘의 음력 날짜 문자열을 (음) %Y-%m-%d 형식으로 반환"
  (let* ((date-list (decode-time (current-time)))
         (year (format "%04d" (nth 5 date-list)))
         (month (format "%02d" (nth 4 date-list)))
         (day (format "%02d" (nth 3 date-list)))
         (lunar (kasi-get-lunar-date year month day)))
    (if lunar
        (let* ((lunar-day (plist-get lunar :day))
               (leap-str (if (string= (plist-get lunar :leap) "윤") " (윤)" "")))
          (format " (음) %s-%s-%s%s"
                  (plist-get lunar :year)
                  (plist-get lunar :month)
                  lunar-day
                  leap-str))
      " (음력 조회 실패)")))

;; ======================================
;;; Tide API Functions (만조/간조 시각 조회용)
;; ======================================
(defvar nokdong-tide-obs-code "SO_0761" 
  "조석예보(고, 저조) API에 따른 녹동항 관측소 코드") 

(defun nokdong-tide-parse-json (json-string)
  "API JSON 문자열을 파싱하여 조석예보 데이터 추출"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (data (condition-case err (json-read-from-string json-string) (error nil))) 
         (header (if (hash-table-p data) (gethash "header" data) nil))
         (body (if (hash-table-p data) (gethash "body" data) nil)))

    (if (not (hash-table-p data)) 
        nil 
      (if (and header (not (string= (gethash "resultCode" header) "00")))
          nil ; API 에러 시 nil 반환
        (let* ((items (if (hash-table-p body) (gethash "items" body) nil))
               (tide-data (if (hash-table-p items) (gethash "item" items) nil)))
          (if (null tide-data)
              (list :data nil)
            (list :data (if (listp tide-data) tide-data (list tide-data)))))))))

(defun nokdong-tide-format-tide-times (tide-data date-str)
  "API 결과를 (High-times-string . Low-times-string) 형태로 반환"
  (let* ((target-date-str (format "%s-%s-%s" 
                                  (substring date-str 0 4)
                                  (substring date-str 4 6)
                                  (substring date-str 6 8)))
         (high-times '()) 
         (low-times '()))
    
    (dolist (item tide-data)
      (let* ((predc-dt (gethash "predcDt" item)) ; 예측일시 (YYYY-MM-DD HH:MM)
             (predc-date-part (if predc-dt (substring predc-dt 0 10) nil)) 
             (extr-se (gethash "extrSe" item)) ; 극치구분 (1, 3: 고조 / 2, 4: 저조)
             (time-only (if predc-dt (substring predc-dt 11 16) nil)))

        ;; 요청 날짜와 일치하는 데이터만 필터링
        (when (and time-only (string-equal predc-date-part target-date-str))
          (cond 
           ((or (string-equal extr-se "1") (string-equal extr-se "3")) 
            (push time-only high-times))
           
           ((or (string-equal extr-se "2") (string-equal extr-se "4")) 
            (push time-only low-times))))))

    ;; 시간순으로 정렬
    (setq high-times (sort high-times 'string<))
    (setq low-times (sort low-times 'string<))
    
    (let ((high-time-str (mapconcat 'identity high-times " "))
          (low-time-str (mapconcat 'identity low-times " ")))
      
      (cons high-time-str low-time-str))))

(defun nokdong-tide-fetch-tide-times (date-str)
  "API를 통해 조석 시각을 가져와 (만조시각문자열 . 간조시각문자열) 형태로 반환"
  (let* ((api-url "https://apis.data.go.kr/1192136/tideFcstHghLw/GetTideFcstHghLwApiService") 
         (final-api-key kasi-api-key) 
         (url (format "%s?serviceKey=%s&obsCode=%s&reqDate=%s&type=json&numOfRows=10"
                      api-url
                      final-api-key 
                      nokdong-tide-obs-code
                      date-str))
         (url-request-method "GET")
         (buffer (url-retrieve-synchronously url t))
         result)
    
    (if buffer
        (unwind-protect
            (with-current-buffer buffer
              (set-buffer-file-coding-system 'utf-8) 
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (forward-char)
              (delete-region (point-min) (point))
              
              (let* ((json-string (buffer-string))
                     (parsed-data (nokdong-tide-parse-json json-string))
                     (tide-data (plist-get parsed-data :data)))
                
                (if (and parsed-data tide-data)
                    (setq result (nokdong-tide-format-tide-times tide-data date-str)))))
          (kill-buffer buffer))
      (message "Tide API 통신 실패"))
    result))

;; ======================================
;;; Main Tide Formatting Function (API 연동)
;; ======================================
(defun my-format-tide-info ()
  "오늘의 조석(만조/간조) 정보와 물때를 리스트로 반환합니다.
반환 형식: (조석시각문자열 . 물때문자열)"
  (let* ((date-list (decode-time (current-time)))
         (year (format "%04d" (nth 5 date-list)))
         (month (format "%02d" (nth 4 date-list)))
         (day (format "%02d" (nth 3 date-list)))
         (date-str (format "%s%s%s" year month day)) ; YYYYMMDD
         (lunar (kasi-get-lunar-date year month day))
         (muldae-str "물때 정보 없음")
         (tide-string ""))
    
    ;; 1. KASI API: Get Muldae
    (if lunar
        (let* ((lunar-day (plist-get lunar :day))
               (muldae (my-calculate-muldae lunar-day)))
          (setq muldae-str (if (= muldae 0) "조금" (format "%d물" muldae)))))

    ;; 2. Tide API: Get High/Low times
    (let ((tide-times (nokdong-tide-fetch-tide-times date-str)))
      (if tide-times
          (let ((high-str (car tide-times))
                (low-str (cdr tide-times)))
            ;; 요청 형식: "- 만조: HH:MM HH:MM\n- 간조: HH:MM HH:MM"
            (setq tide-string
                  (format "- 만조: %s\n- 간조: %s"
                          (if (string-empty-p high-str) "정보 없음" high-str)
                          (if (string-empty-p low-str) "정보 없음" low-str))))
        (setq tide-string "- 조석 정보 조회 실패 (API)"))
      
      ;; Combine Muldae and Tide string
      (cons tide-string (format " (%s)" muldae-str)))))

;; ======================================
;;; Helper Functions
;; ======================================
(defun get-random-quote-from-creading ()
  "Extract a random quote from the cReading.org file."
  (with-temp-buffer
    (insert-file-contents (my-org-person-file-path "cReading.org"))
    (goto-char (point-min))
    (let ((quotes '()))
      (while (re-search-forward "^\\* \\(.+\\)" nil t)
        (let* ((title (match-string 1))
               (content (string-trim
                         (buffer-substring-no-properties
                          (point)
                          (save-excursion
                            (if (re-search-forward "^\\* " nil t)
                                (match-beginning 0)
                              (point-max)))))))
          (push (cons title content) quotes)))
      (if quotes
          (let ((quote (nth (random (length quotes)) quotes)))
            (format "%s\n%s" (car quote) (cdr quote)))
        "No quotes found in cReading.org"))))

(defun my-emacs-copyright ()
  "Return the copyright information for Emacs with current year."
  (format "Copyright © 1996-%s,  Free Software Foundation, Inc."
          (format-time-string "%Y")))

(defun my-Ddays ()
  "Calculate days remaining until or elapsed since 2024-12-31."
  (let* ((today (current-time))
         (target-date (encode-time 0 0 0 31 12 2024))
         (diff-seconds (float-time (time-subtract today target-date)))
         (diff-days (floor (/ diff-seconds 86400))))
    (if (> diff-days 0)
        (format "/  %d日 경과" diff-days)
      (format "/ D-day %d日前" (- diff-days)))))

(defun my-format-agenda-string ()
  "Get formatted 3-day agenda string with bullet points.
Now includes holidays from my-calendar.el via org-agenda integration."
  (with-temp-buffer
    (org-agenda-list 3)
    (goto-char (point-min))
    (forward-line 1)
    (replace-regexp-in-string
      "^" "- "
      (buffer-substring-no-properties (point) (point-max)))))


;; ======================================
;;; Main Function
;; ======================================
(defun my-todays-pop ()
  "Display today's date (solar and lunar), agenda, and random quote in a popup buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "Today info"))
         (current-date (format-time-string "● 오늘 %Y-%m-%d (%A)"))
         (lunar-date-string (my-lunar-date-string))
         (d-day (my-Ddays))
         (tide-result (my-format-tide-info)) ; (조석시각문자열 . 물때문자열)
         (tide-info (car tide-result))
         (muldae-str (cdr tide-result)) ; 예: " (1물)"
         (agenda-string (my-format-agenda-string))
         (random-quote (get-random-quote-from-creading)))

    (with-current-buffer buffer
      (erase-buffer)

      ;; Header
      (fancy-splash-head)
      ;; 8칸 들여쓰기 복원
      (insert "        " (my-emacs-copyright) "\n") 

      ;; Date: 양력 + 음력 + D-day
      ;; 4칸 들여쓰기 복원
      (insert "    " current-date lunar-date-string " " d-day)

      ;; Tide Info (조석 정보) - 물때 추가
      ;; 4칸 들여쓰기 복원 (헤더)
      (insert (format "\n    ● 조석%s\n" muldae-str)) 
      ;; 8칸 들여쓰기 복원 (내용)
      (insert (replace-regexp-in-string "^" "        " tide-info))

      ;; Agenda (holidays.org의 기념일/절기 포함)
      ;; 4칸 들여쓰기 복원 (헤더)
      (insert (format "\n    ● 일정\n"))
      ;; 8칸 들여쓰기 복원 (내용)
      (insert (replace-regexp-in-string "^" "        " agenda-string))

      ;; Quote
      ;; 4칸 들여쓰기 복원 (헤더)
      (insert (format "\n    ● 글말\n"))
      ;; 8칸 들여쓰기 복원 (내용)
      (insert (replace-regexp-in-string "^" "        " random-quote))

      ;; Cursor position
      (goto-char (point-min))
      (forward-line 1)
      (beginning-of-line)

      ;; Key binding
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q")
          (lambda () (interactive) (kill-this-buffer)))
        (use-local-map map)))

    (switch-to-buffer buffer)))


;;; end here
(provide 'my-todays-pop)
