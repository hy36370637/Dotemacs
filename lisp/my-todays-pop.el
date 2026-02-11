;;; my-todays-pop.el --- Today's information display -*- lexical-binding: t; -*-

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
(require 'dom)
(require 'my-search)

;; ======================================
;;; Configuration Variables
;; ======================================
(defvar kasi-api-key nil
  "KASI API key for lunar calendar and tide data.")

(defvar nokdong-tide-obs-code "SO_0761" 
  "Nokdong port observation code for tide forecast API.")

(defvar my-weather-location "도양읍" 	;도양읍
  "Default location for weather information.")

(defvar my-weather-format-template "- %s, 최저/최고 %s/%s, 어제보다 %s"
  "Weather display format template.
Format placeholders:
  1st %s = weather status (e.g., '오전 10%% 맑음 오후 10%% 맑음')
  2nd %s = lowest temperature (e.g., '-2°')
  3rd %s = highest temperature (e.g., '11°')
  4th %s = comparison with yesterday (e.g., '3° 높아요')")

(defvar my--weather-cache nil
  "Cache for weather data: (timestamp . weather-string)")

;; ======================================
;;; KASI API Functions
;; ======================================
(defun kasi-load-api-key ()
  "Load API key from lunar_api file."
  (let ((key-file (expand-file-name "lunar_api" 
                                    (file-name-directory 
                                     (or load-file-name buffer-file-name)))))
    (unless (file-exists-p key-file)
      (error "lunar_api 파일을 찾을 수 없습니다: %s" key-file))
    (with-temp-buffer
      (insert-file-contents key-file)
      (setq kasi-api-key (string-trim (buffer-string))))))

;; Initialize API key on load
(unless kasi-api-key
  (kasi-load-api-key))


(defun kasi-get-xml-text (item-node tag)
  "Extract text content from TAG in ITEM-NODE."
  (when-let ((child (car (xml-get-children item-node tag))))
    (car (xml-node-children child))))


(defun kasi-get-lunar-date (year month day)
  "Fetch lunar date for given YEAR, MONTH, DAY from KASI API."
  (unless kasi-api-key (kasi-load-api-key))
  (when-let ((buffer (url-retrieve-synchronously 
                      (format "http://apis.data.go.kr/B090041/openapi/service/LrsrCldInfoService/getLunCalInfo?solYear=%s&solMonth=%s&solDay=%s&ServiceKey=%s"
                              year month day kasi-api-key)
                      t nil 5)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (when-let* ((xml-data (xml-parse-region (point) (point-max)))
                        (response (car xml-data))
                        (body (car (xml-get-children response 'body)))
                        (items (car (xml-get-children body 'items)))
                        (item (car (xml-get-children items 'item))))
              (list :year (kasi-get-xml-text item 'lunYear)
                    :month (kasi-get-xml-text item 'lunMonth)
                    :day (kasi-get-xml-text item 'lunDay)
                    :leap (kasi-get-xml-text item 'lunLeapmonth)))))
      (kill-buffer buffer))))


(defun my-calculate-muldae (lunar-day)
  "Calculate tide cycle position from LUNAR-DAY (8-cycle system for Nokdong)."
  (let ((day (string-to-number lunar-day)))
    (cond
     ((<= 1 day 7) (+ day 7))
     ((= day 8) 0)
     ((<= 9 day 15) (- day 8))
     ((<= 16 day 22) (+ (- day 16) 8))
     ((= day 23) 0)
     ((<= 24 day 30) (- day 23))
     (t 1))))


(defun my-lunar-date-string ()
  "Return today's lunar date string in format: (음) YYYY-MM-DD."
  (let* ((now (decode-time))
         (year (format "%04d" (nth 5 now)))
         (month (format "%02d" (nth 4 now)))
         (day (format "%02d" (nth 3 now)))
         (lunar (kasi-get-lunar-date year month day)))
    (if lunar
        (format " (음) %s-%s-%s%s"
                (plist-get lunar :year)
                (plist-get lunar :month)
                (plist-get lunar :day)
                (if (string= (plist-get lunar :leap) "윤") " (윤)" ""))
      " (음력 조회 실패)")))

;; ======================================
;;; Tide API Functions
;; ======================================
(defun nokdong-tide-parse-json (json-string)
  "Parse tide forecast JSON-STRING and extract data."
  (condition-case nil
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (data (json-read-from-string json-string))
             (header (gethash "header" data))
             (body (gethash "body" data)))
        (when (string= (gethash "resultCode" header) "00")
          (when-let* ((items (gethash "items" body))
                      (tide-data (gethash "item" items)))
            (list :data (if (listp tide-data) tide-data (list tide-data))))))
    (error nil)))


(defun nokdong-tide-format-tide-times (tide-data date-str)
  "Format TIDE-DATA for DATE-STR into (HIGH-TIMES . LOW-TIMES) cons."
  (let ((target-date (format "%s-%s-%s" 
                             (substring date-str 0 4)
                             (substring date-str 4 6)
                             (substring date-str 6 8)))
        high-times low-times)
    (dolist (item tide-data)
      (when-let* ((predc-dt (gethash "predcDt" item))
                  (predc-date (substring predc-dt 0 10))
                  (time-only (substring predc-dt 11 16))
                  ((string= predc-date target-date)))
        (let ((extr-se (gethash "extrSe" item)))
          (cond 
           ((member extr-se '("1" "3")) (push time-only high-times))
           ((member extr-se '("2" "4")) (push time-only low-times))))))
    (cons (string-join (sort high-times #'string<) " ")
          (string-join (sort low-times #'string<) " "))))


(defun nokdong-tide-fetch-tide-times (date-str)
  "Fetch tide times for DATE-STR and return (HIGH-TIMES . LOW-TIMES)."
  (when-let ((buffer (url-retrieve-synchronously 
                      (format "https://apis.data.go.kr/1192136/tideFcstHghLw/GetTideFcstHghLwApiService?serviceKey=%s&obsCode=%s&reqDate=%s&type=json&numOfRows=5"
                              kasi-api-key nokdong-tide-obs-code date-str)
                      t nil 5)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (forward-char)
            (delete-region (point-min) (point))
            (when-let* ((parsed (nokdong-tide-parse-json (buffer-string)))
                        (tide-data (plist-get parsed :data)))
              (nokdong-tide-format-tide-times tide-data date-str))))
      (kill-buffer buffer))))


(defun my-format-tide-info (&optional days-offset)
  "음력(물때)과 조석 시각을 각각 독립적으로 시도하여 결과를 반환합니다."
  (let* ((offset (or days-offset 0))
         (target-time (time-add (current-time) (days-to-time offset)))
         (decoded (decode-time target-time))
         (year (format "%04d" (nth 5 decoded)))
         (month (format "%02d" (nth 4 decoded)))
         (day (format "%02d" (nth 3 decoded)))
         (date-str (concat year month day))
         ;; 1. 음력 데이터 가져오기 (실패해도 진행)
         (lunar (condition-case nil (kasi-get-lunar-date year month day) (error nil)))
         ;; 2. 조석 시각 가져오기 (실패해도 진행)
         (tide-times (condition-case nil (nokdong-tide-fetch-tide-times date-str) (error nil)))
         (muldae-str " (물때 정보 없음)")
         (tide-string "- 조석 정보 조회 실패"))

    ;; 물때 계산 (음력이 있을 때만)
    (when lunar
      (let ((muldae (my-calculate-muldae (plist-get lunar :day))))
        (setq muldae-str (format " (%s)" (if (zerop muldae) "조금" (format "%d물" muldae))))))
    
    ;; 조석 시각 포맷팅 (조석 정보가 있을 때만)
    (when tide-times
      (let ((high (if (string-empty-p (car tide-times)) "정보 없음" (car tide-times)))
            (low (if (string-empty-p (cdr tide-times)) "정보 없음" (cdr tide-times))))
        (setq tide-string (format "- 간조: %s\n- 만조: %s" low high))))

    (cons tide-string muldae-str)))

;; ======================================
;;; Weather Functions
;; ======================================
(defun my--format-weather-inline (dom)
  "Extract and format weather for inline display in date line."
  (when-let* ((weekly-forecast (dom-by-class dom "week_item"))
              (day (car weekly-forecast))
              (weather-elem (car (dom-by-class day "weather")))
              (weather-raw (dom-texts weather-elem))
              (weather-normalized (string-trim (replace-regexp-in-string "\\s-+" " " weather-raw)))
              (lowest-elem (car (dom-by-class day "lowest")))
              (highest-elem (car (dom-by-class day "highest")))
              (lowest (string-trim (string-replace "최저기온" "" (dom-texts lowest-elem))))
              (highest (string-trim (string-replace "최고기온" "" (dom-texts highest-elem)))))
    
    (let* ((yesterday-day (nth 1 weekly-forecast))
           (yesterday-highest-elem (car (dom-by-class yesterday-day "highest")))
           (yesterday-high (if yesterday-highest-elem
                               (string-to-number 
                                (string-trim (string-replace "최고기온" "" (dom-texts yesterday-highest-elem))))
                             (string-to-number highest)))
           (today-high (string-to-number highest))
           (temp-diff (- today-high yesterday-high))
           (comparison (cond
                        ((> temp-diff 0) (format "%d° 높아요" temp-diff))
                        ((< temp-diff 0) (format "%d° 낮아요" (abs temp-diff)))
                        (t "어제와 같아요"))))
      (format my-weather-format-template weather-normalized lowest highest comparison))))


(defun my--format-weekly-weather (dom)
  "Extract and format weekly weather forecast from DOM."
  (when-let* ((weekly-forecast (dom-by-class dom "week_item"))
              (weekly-data (mapcar 
                            (lambda (day)
                              (let* ((date (string-trim (dom-texts (dom-by-class day "date"))))
                                     (weather-raw (string-trim (dom-texts (dom-by-class day "weather"))))
                                     (weather (replace-regexp-in-string "\\s-+" " " weather-raw))
                                     (lowest (string-trim (string-replace "최저기온" "" 
                                                                          (dom-texts (dom-by-class day "lowest")))))
                                     (highest (string-trim (string-replace "최고기온" "" 
                                                                           (dom-texts (dom-by-class day "highest"))))))
                                (format "- %s: %s, 최저/최고 %s/%s" date weather lowest highest)))
                            (seq-take (cdr weekly-forecast) 4))))  ; Skip today, take next 6 days
    (string-join weekly-data "\n")))


(defun my--get-weather-info-sync ()
  "Get weather info synchronously with caching (5-minute cache).
Returns (TODAY-WEATHER . WEEKLY-WEATHER) cons cell."
  (let ((current-time (float-time)))
    (if (and my--weather-cache
             (< (- current-time (car my--weather-cache)) 300))
        (cdr my--weather-cache)
      (when-let ((buffer (url-retrieve-synchronously 
                          (format "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=%s%%20날씨"
                                  (url-hexify-string my-weather-location))
                          t nil 5)))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (when (re-search-forward "^$" nil t)
                (let* ((dom (libxml-parse-html-region (point) (point-max)))
                       (today-str (my--format-weather-inline dom))
                       (weekly-str (my--format-weekly-weather dom))
                       (result (cons today-str weekly-str)))
                  (when (and today-str weekly-str)
                    (setq my--weather-cache (cons current-time result))
                    result))))
          (kill-buffer buffer))))))

;; ======================================
;;; Helper Functions
;; ======================================
(defun get-random-quote-from-creading ()
  "Extract a random quote from cReading.org file and clean up trailing symbols."
  (with-temp-buffer
    (insert-file-contents (my-org-person-file-path "cReading.org"))
    (let (quotes)
      (goto-char (point-min))
      ;; 제목(* 타이틀)과 본문을 분리하여 수집
      (while (re-search-forward "^\\* \\(.+\\)" nil t)
        (let* ((title (match-string 1))
               (start (point))
               (end (save-excursion 
                      (if (re-search-forward "^\\* " nil t) 
                          (match-beginning 0) 
                        (point-max))))
               (content (string-trim (buffer-substring-no-properties start end))))
          (push (cons title content) quotes)))
      (if quotes
          (let* ((quote (nth (random (length quotes)) quotes))
                 (raw-result (format "%s\n%s" (car quote) (cdr quote))))
            ;; 결과값 끝에 있는 '*', 공백, 개행을 모두 제거
            (replace-regexp-in-string "[ \t\n\*]+$" "" raw-result))
        "No quotes found in cReading.org"))))



(defun my-format-agenda-string ()
  "Get formatted 3-day agenda with bullets, removing trailing empty lines."
  (with-temp-buffer
    (org-agenda-list 5)
    (goto-char (point-min))
    (forward-line 1) ; 첫 줄(헤더) 제외
    (let ((content (buffer-substring-no-properties (point) (point-max))))
      ;; 마지막 개행 및 공백 제거 후 각 줄 처음에 "- " 추가
      (replace-regexp-in-string "^" "- " (string-trim-right content)))))

;; ======================================
;;; Main Function
;; ======================================
(defun my-todays-pop ()
  "Display today's and tomorrow's info in a popup buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*Today info*"))
         (current-date (format-time-string "● 오늘: %Y-%m-%d (%a) /"))
         (lunar-date (my-lunar-date-string))
         (weather-data (my--get-weather-info-sync))
         (today-weather (when weather-data (car weather-data)))
         (weekly-weather (when weather-data (cdr weather-data)))
         
         ;; 오늘 및 내일 조석 정보 가져오기
         (tide-today (my-format-tide-info 0))
         (tide-tomorrow (my-format-tide-info 1))
         
         (agenda (my-format-agenda-string))
         (quote (get-random-quote-from-creading))
         (indent-4 "    ")
         (indent-8 "        "))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert indent-8 (my/emacs-copyright) "\n\n")
        
        ;; Date
        (insert indent-4 current-date lunar-date "\n")
        
        ;; Weather Section
        (when (and today-weather weekly-weather)
          (insert (format "%s● 날씨: %s\n" indent-4 my-weather-location))
          (insert indent-8 today-weather "\n")
          (insert (replace-regexp-in-string "^" indent-8 weekly-weather)))
        
        ;; Tide Section (Today & Tomorrow)
        (insert (format "\n%s● 조석 (오늘): %s\n" indent-4 
                        (substring (cdr tide-today) 2 -1)))
        (insert (replace-regexp-in-string "^" indent-8 (car tide-today)))
        
        (insert (format "\n%s● 조석 (내일): %s\n" indent-4 
                        (substring (cdr tide-tomorrow) 2 -1)))
        (insert (replace-regexp-in-string "^" indent-8 (car tide-tomorrow)))
        
        ;; Agenda
        (insert (format "\n%s● 일정\n" indent-4))
        (insert (replace-regexp-in-string "^" indent-8 agenda))
        
        ;; Quote
        (insert (format "\n%s● 글말: " indent-4))
        (let* ((quote-lines (split-string quote "\n"))
               (title (car quote-lines))
               (content-lines (cdr quote-lines)))
          (insert (format "%s\n" (or title "")))
          (when content-lines
            (dolist (line content-lines)
              (let ((trimmed (string-trim line)))
                (unless (string-empty-p trimmed)
                  (let ((formatted-line (if (string-prefix-p "-" trimmed)
                                            trimmed
                                          (concat "- " trimmed))))
                    (insert (format "%s%s\n" indent-8 formatted-line))))))))
        
        ;; Setup buffer
        (goto-char (point-min))
        (forward-line 1)
        (beginning-of-line)
        
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "q") 
            (lambda () (interactive) (kill-this-buffer)))
          (use-local-map map))))

    (switch-to-buffer buffer)))



(provide 'my-todays-pop)
;;; my-todays-pop.el ends here
