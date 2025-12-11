;;; my-todays-pop.el --- Today's information display -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; ======================================
;;; KASI API Settings and Helper
;; ======================================
(require 'url)
(require 'xml)

;; API 키를 파일에서 읽어옵니다
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
      (error "api 파일을 찾을 수 없습니다: %s" key-file))))

;; 초기화 시 API 키 로드
(unless kasi-api-key
  (kasi-load-api-key))

(defun kasi-get-xml-text (item-node tag)
  "주어진 XML 노드(item-node)에서 특정 태그(tag)의 텍스트 콘텐츠를 추출."
  (when item-node
    (car (xml-node-children (car (xml-get-children item-node tag))))))

(defun kasi-get-lunar-date (year month day)
  "KASI API로 양력을 음력으로 변환"
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

(defun my-lunar-date-string ()
  "음력 날짜 문자열, (음) %Y-%m-%d 형식으로 반환."
  (let* ((date-list (decode-time (current-time)))
         (year (format "%04d" (nth 5 date-list)))
         (month (format "%02d" (nth 4 date-list)))
         (day (format "%02d" (nth 3 date-list)))
         (lunar (kasi-get-lunar-date year month day)))
    (if lunar
        (format " (음) %s-%s-%s%s"
                (plist-get lunar :year)
                (plist-get lunar :month)
                (plist-get lunar :day)
                (if (string= (plist-get lunar :leap) "윤") " (윤)" ""))
      " (음력 조회 실패 - API 오류)")))

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
  "Get formatted 3-day agenda string with bullet points."
  (with-temp-buffer
    (org-agenda-list 3)
    (goto-char (point-min))
    (forward-line 1)
    (replace-regexp-in-string
      "^" "- "
      (buffer-substring-no-properties (point) (point-max)))))

;; ======================================
;;; Main Function (Updated)
;; ======================================
(defun my-todays-pop ()
  "Display today's date (solar and lunar), agenda, and random quote in a popup buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "Today 오늘.."))
         (current-date (format-time-string "● 오늘 %Y-%m-%d (%A)"))
         (lunar-date-string (my-lunar-date-string))
         (d-day (my-Ddays))
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

      ;; Agenda
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
