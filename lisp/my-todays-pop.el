;;; my-todays-pop.el --- Today's information display -*- lexical-binding: t; -*-
;; Author: Ho Young <hy36370637@gmail.com>

;;; Commentary:
;; Display today's date, agenda, and random quotes from cReading.org
;; Holidays are now integrated via org-agenda (no separate holiday lookup needed)

;;; Code:
;; ======================================
;;; KASI API Settings and Helper
;; ======================================
(require 'url)
(require 'xml)

;; API 키 
(defvar kasi-api-key nil
  "KASI API 키")

(defun kasi-load-api-key ()
  (let ((key-file (expand-file-name "lunar_api" 
                                     (file-name-directory 
                                      (or load-file-name buffer-file-name)))))
    (if (file-exists-p key-file)
        (with-temp-buffer
          (insert-file-contents key-file)
          (setq kasi-api-key (string-trim (buffer-string))))
      (error "lunar_api 파일을 찾을 수 없습니다: %s" key-file))))

;; 초기화 시 API 키 로드
(unless kasi-api-key
  (kasi-load-api-key))

(defun kasi-get-xml-text (item-node tag)
  "주어진 XML 노드(item-node)에서 특정 태그(tag)의 텍스트 콘텐츠를 추출합니다."
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
  "음력 날짜로 물때를 계산합니다 (남해안 녹동항 기준 - 8물때식).
음력 1일, 16일 = 8물 (사리) 음력 8일, 23일 = 조금 음력 9일, 24일 = 1물"
  (let* ((day (string-to-number lunar-day))
         ;; 8물때식 계산
         ;; 음력 1일부터: 8,9,10,11,12,13,14,조금,1,2,3,4,5,6,7
         ;;                8,9,10,11,12,13,14,조금,1,2,3,4,5,6,7
         (cycle-pos (cond
                     ;; 음력 1~7일: 8물,9물,10물,11물,12물,13물,14물
                     ((= day 1) 8)
                     ((= day 2) 9)
                     ((= day 3) 10)
                     ((= day 4) 11)
                     ((= day 5) 12)
                     ((= day 6) 13)
                     ((= day 7) 14)
                     ;; 음력 8일: 조금
                     ((= day 8) 0)
                     ;; 음력 9~15일: 1물,2물,3물,4물,5물,6물,7물
                     ((<= 9 day 15) (- day 8))
                     ;; 음력 16~22일: 8물,9물,10물,11물,12물,13물,14물
                     ((= day 16) 8)
                     ((= day 17) 9)
                     ((= day 18) 10)
                     ((= day 19) 11)
                     ((= day 20) 12)
                     ((= day 21) 13)
                     ((= day 22) 14)
                     ;; 음력 23일: 조금
                     ((= day 23) 0)
                     ;; 음력 24~30일: 1물,2물,3물,4물,5물,6물,7물
                     ((<= 24 day 30) (- day 23))
                     (t 1))))
    cycle-pos))

(defun my-calculate-tide-times (lunar-day)
  "음력일을 기준으로 만조/간조 시각을 계산합니다.
녹동항 실제 데이터(음력 10.24, 1물)를 기준점으로 사용:
- 만조: 04:47, 16:45
- 간조: 10:51, 23:43
매일 약 50분씩 늦어지는 원리를 적용합니다."
  (let* ((day (string-to-number lunar-day))
         ;; 기준점: 음력 24일 만조1 04:47 = 4.783시간
         ;; 역산하여 음력 1일 만조 시각: 약 09:42
         (base-hour (+ 9.7 (* (- day 1) 0.83)))
         (high1-hour (mod (floor base-hour) 24))
         (high1-min (round (* (- base-hour (floor base-hour)) 60)))
         (high2-hour (mod (+ high1-hour 12) 24))
         (high2-min high1-min)
         ;; 간조1: 만조1 + 약 6시간 4분 (실제 04:47→10:51)
         (low1-base (+ base-hour 6.067))
         (low1-hour (mod (floor low1-base) 24))
         (low1-min (round (* (- low1-base (floor low1-base)) 60)))
         ;; 간조2: 만조2 + 약 6시간 58분 (실제 16:45→23:43)
         (low2-base (+ base-hour 12 6.967))
         (low2-hour (mod (floor low2-base) 24))
         (low2-min (round (* (- low2-base (floor low2-base)) 60))))
    (list :high1 (format "%02d:%02d" high1-hour high1-min)
          :high2 (format "%02d:%02d" high2-hour high2-min)
          :low1 (format "%02d:%02d" low1-hour low1-min)
          :low2 (format "%02d:%02d" low2-hour low2-min))))

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

(defun my-format-tide-info ()
  "오늘의 조석(만조/간조) 정보와 물때를 리스트로 반환합니다.
반환 형식: (조석시각문자열 . 물때문자열)"
  (let* ((date-list (decode-time (current-time)))
         (year (format "%04d" (nth 5 date-list)))
         (month (format "%02d" (nth 4 date-list)))
         (day (format "%02d" (nth 3 date-list)))
         (lunar (kasi-get-lunar-date year month day)))
    (if lunar
        (let* ((lunar-day (plist-get lunar :day))
               (muldae (my-calculate-muldae lunar-day))
               (muldae-str (if (= muldae 0) "조금" (format "%d물" muldae)))
               (tide-times (my-calculate-tide-times lunar-day))
               (tide-string (format "- 만조: %s, %s\n- 간조: %s, %s"
                                    (plist-get tide-times :high1)
                                    (plist-get tide-times :high2)
                                    (plist-get tide-times :low1)
                                    (plist-get tide-times :low2))))
          (cons tide-string (format " (%s)" muldae-str)))
      (cons "- 조석 정보 조회 실패" ""))))

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
  (format "Copyright © 1996-%s,  Free Software Foundation, Inc."
          (format-time-string "%Y")))

(defun my-Ddays ()
  "Calculate days remaining until or elapsed since 2024-12-31."
  (let* ((today (current-time))
         (target-date (encode-time 0 0 0 31 12 2024))
         (diff-seconds (float-time (time-subtract today target-date)))
         (diff-days (floor (/ diff-seconds 86400))))
    (if (> diff-days 0)
        (format "/  %d日 경과" diff-days)
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
         (random-quote (get-random-quote-from-creading))
         (left-margin "    ")
         (quote-margin "        "))

    (with-current-buffer buffer
      (erase-buffer)

      ;; Header
      (fancy-splash-head)
      (insert quote-margin (my-emacs-copyright) "\n")

      ;; Date: 양력 + 음력 + D-day
      (insert left-margin current-date lunar-date-string " " d-day)

      ;; Tide Info (조석 정보) - 물때 추가
      (insert (format "\n%s● 조석%s\n" left-margin muldae-str)) ; 물때
      (insert (replace-regexp-in-string "^" quote-margin tide-info))

      ;; Agenda (holidays.org의 기념일/절기 포함)
      (insert (format "\n%s● 일정\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin agenda-string))

      ;; Quote
      (insert (format "\n%s● 글말\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin random-quote))

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
