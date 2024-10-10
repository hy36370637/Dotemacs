;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-todays-pop.el
(defun my-Ddays ()
  "Calculate the number of days until 2024-12-31."
  (let* ((today (current-time))
         (target-date (encode-time 0 0 0 31 12 2024))
         (diff-days (floor (/ (float-time (time-subtract target-date today)) 86400))))
    (format "/ D-day %d 日前" diff-days)))

(defun my-todays-pop ()
  "today 정보. 일자, 일정 등"
  (interactive)  ;; 함수가 인터랙티브로 동작하도록 설정
  (let ((buffer (get-buffer-create "Today's Happy"))
        (current-date (format-time-string "● 오늘 %Y-%m-%d (%A) "))
        (d-day (my-Ddays))  ;; D-day 계산
        (agenda-string (with-temp-buffer
                         (org-agenda-list 3)  ;; 오늘의 일정만 가져오기
                         (buffer-string)))     ;; 일정을 문자열로 변환
        (random-quote (get-random-quote-from-creading))  ;; 무작위 인용구 가져오기
        (left-margin "    "))  ;; 왼쪽 여백을 위한 변수
    (with-current-buffer buffer
      (erase-buffer)  ;; 기존 내용을 지우고
      (fancy-splash-head)  ;; 기본 로고 출력
      (insert "\n")  ;; 로고와 다른 내용 사이에 줄 바꿈 추가
      (insert left-margin (my-emacs-copyright))  ;; 사용자 로고 출력
      (insert "\n")  ;; 로고와 다른 내용 사이에 줄 바꿈 추가
      (insert left-margin current-date d-day)  ;; 날짜 삽입
;;      (insert (format "%s* 남은 날짜: %s\n\n" left-margin d-day))  ;; D-day 삽입
      (insert (format "\n%s● 오늘의 일정:\n" left-margin))
      (insert (replace-regexp-in-string "^" left-margin agenda-string))  ;; 일정 삽입 (각 줄마다 여백 추가)
      (insert (format "\n%s● 오늘의 인용구:\n" left-margin))
      (insert (replace-regexp-in-string "^" left-margin random-quote))  ;; 인용구 삽입 (각 줄마다 여백 추가)
      (goto-char (point-min)))  ;; 커서를 버퍼의 맨 처음으로 이동
    (switch-to-buffer buffer)))  ;; 버퍼 전환

;; 단축키 바인딩 예시 (F8)
(global-set-key (kbd "s-3") 'my-todays-pop)
