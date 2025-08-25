;;; -*- lexical-binding: t; -*-
;; /.emacs.d/lisp/my-calendar.el

;; ======================================
;;; holidays/calendar
;; ======================================
(defun my-market-days ()
  "Generate market day holidays for current and next month only."
  (let ((holidays '())
        (current-month (string-to-number (format-time-string "%m")))
        (current-year (string-to-number (format-time-string "%Y"))))
    ;; 현재 월
    (dolist (day '(3 8 13 18 23 28))
      (push (list current-month day "녹동장") holidays))
    ;; 다음 월 (12월이면 1월로, 연도도 증가)
    (let ((next-month (if (= current-month 12) 1 (1+ current-month))))
      (dolist (day '(3 8 13 18 23 28))
        (push (list next-month day "녹동장") holidays)))
    holidays))

(defun my/setup-calendar-holidays ()
  "Set up custom holidays and solar terms for the calendar."
  (setq my-holidays
        '((holiday-fixed 1 29 "딸日")
          (holiday-fixed 3 19 "결혼日")
          (holiday-fixed 6 10 "아들日")))
  
  ;; 24절기 설정
  (setq 24solar-holidays
        '((holiday-fixed 2 4 "입춘")
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
          (holiday-fixed 12 22 "동지")
          (holiday-fixed 1 5 "소한")
          (holiday-fixed 1 20 "대한")))
  
  ;; 장날 설정 (매월 3, 8, 13, 18, 23, 28일)
  (setq market-day-holidays
        (mapcar (lambda (day-info)
                  (list 'holiday-fixed (nth 0 day-info) (nth 1 day-info) (nth 2 day-info)))
                (my-market-days)))
  
  ;; 휴일 설정 초기화 및 적용
  (setq holiday-general-holidays nil
        holiday-local-holidays nil
        holiday-other-holidays nil
        holiday-christian-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        calendar-mark-holidays-flag t)
  
  ;; 모든 휴일 통합
  (setq calendar-holidays (append my-holidays 
                                  24solar-holidays 
                                  market-day-holidays)))

(defun cal-fixLayout ()
  "calendar layout for D2Coding font. 고정폭 깨짐방지"
  (face-remap-add-relative 'default '(:family "Noto Sans Mono CJK KR" :height 160)))

(use-package calendar
  :ensure nil
  :hook ((calendar-mode . cal-fixLayout)
         ((calendar-mode org-agenda-mode) . my/setup-calendar-holidays))
  :config
  (setq calendar-week-start-day 0	;일요일부터 시작
;;	calendar-day-header-array ["일" "월" "화" "수" "목" "금" "토"]
        calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"])
  (custom-set-faces
   '(holiday ((t (:foreground "red" :weight bold))))))



;;; end here
(provide 'my-calendar)
