;;; my-calendar.el --- Calendar and holidays configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom calendar setup with Korean holidays, solar terms, and market days

;;; Code:

;; ======================================
;;; Helper Functions
;; ======================================
;; (defun my-market-days ()
;;   "Generate market day holidays for current and next month only."
;;   (let* ((holidays '())
;;          (current-month (string-to-number (format-time-string "%m")))
;;          (current-year (string-to-number (format-time-string "%Y")))
;;          (next-month (if (= current-month 12) 1 (1+ current-month)))
;;          (market-days '(3 8 13 18 23 28)))
;;     ;; Current month
;;     (dolist (day market-days)
;;       (push (list current-month day "녹동장") holidays))    
;;     ;; Next month
;;     (dolist (day market-days)
;;       (push (list next-month day "녹동장") holidays))
;;     holidays))

(defun my/setup-calendar-holidays ()
  "Set up custom holidays and solar terms for the calendar."
  ;; Personal holidays
  (setq my-holidays
        '((holiday-fixed 1 29 "딸日")
          (holiday-fixed 3 19 "결혼日")
          (holiday-fixed 6 10 "아들日")))
  
  ;; 24 Solar Terms (24절기)
  (setq 24solar-holidays
        '((holiday-fixed 1 5 "소한")
          (holiday-fixed 1 20 "대한")
          (holiday-fixed 2 4 "입춘")
          (holiday-fixed 2 19 "우수")
          (holiday-fixed 3 5 "경칩")
          (holiday-fixed 3 20 "춘분")
          (holiday-fixed 4 5 "청명")
          (holiday-fixed 4 20 "곡우")
          (holiday-fixed 5 5 "입하")
          (holiday-fixed 5 21 "소만")
          (holiday-fixed 6 6 "망종")
          (holiday-fixed 6 21 "하지")
          (holiday-fixed 7 7 "소서")
          (holiday-fixed 7 22 "대서")
          (holiday-fixed 8 7 "입추")
          (holiday-fixed 8 23 "처서")
          (holiday-fixed 9 7 "백로")
          (holiday-fixed 9 22 "추분")
          (holiday-fixed 10 8 "한로")
          (holiday-fixed 10 23 "상강")
          (holiday-fixed 11 7 "입동")
          (holiday-fixed 11 22 "소설")
          (holiday-fixed 12 7 "대설")
          (holiday-fixed 12 22 "동지")))
  
  ;; Market days (장날: 매월 3, 8, 13, 18, 23, 28일)
  ;; (setq market-day-holidays
  ;;       (mapcar (lambda (day-info)
  ;;                 (list 'holiday-fixed 
  ;;                       (nth 0 day-info) 
  ;;                       (nth 1 day-info) 
  ;;                       (nth 2 day-info)))
  ;;               (my-market-days)))
  
  ;; Disable default holidays
  (setq holiday-general-holidays nil
        holiday-local-holidays nil
        holiday-other-holidays nil
        holiday-christian-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        calendar-mark-holidays-flag t)
  
  ;; Combine all holidays
  (setq calendar-holidays (append my-holidays 
                                  24solar-holidays)))
 ;;                                 market-day-holidays)))

(defun cal-fixLayout ()
  "Fix calendar layout for monospace Korean font (D2Coding compatibility)."
  (face-remap-add-relative 'default 
                           '(:family "Noto Sans Mono CJK KR" :height 160)))

;; ======================================
;;; Calendar Configuration
;; ======================================
(use-package calendar
  :ensure nil
  :hook
  ((calendar-mode . cal-fixLayout)
   ((calendar-mode org-agenda-mode) . my/setup-calendar-holidays))
  :custom
  (calendar-week-start-day 0)  ; Start week on Sunday
  (calendar-date-style 'iso)   ; YYYY-MM-DD 형식
  (calendar-month-name-array 
   ["1월" "2월" "3월" "4월" "5월" "6월" 
    "7월" "8월" "9월" "10월" "11월" "12월"])
 ;;	calendar-day-header-array ["일" "월" "화" "수" "목" "금" "토"]
  :config
  ;; 하단 메뉴 텍스트 변경
  (setq calendar-mode-line-format
        (list
         (propertize "<" 'help-echo "이전 달")
;;         "Calendar"
         (propertize "? 정보 / o 달 선택 / . 오늘" 
                     'help-echo "오늘")
         '(calendar-date-string (calendar-current-date) t)
         (propertize ">" 'help-echo "다음 달")))
  (custom-set-faces
   '(holiday ((t (:foreground "red" :weight bold))))))

;;; my-calendar.el ends here
(provide 'my-calendar)
