;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-todays-pop.el

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
          (let* ((quote (nth (random (length quotes)) quotes))
                 (formatted-quote (format "%s\n%s" (car quote) (cdr quote))))
            formatted-quote)
        "No quotes found in cReading.org"))))

(defun my-emacs-copyright ()
  "Return the version･copyright information for Emacs, including the current year."
  (let ((current-year (format-time-string "%Y")))
    (format "Copyright © 1996-%s,  Free Software Foundation, Inc." current-year)))
  ;; (let ((current-year (format-time-string "%Y"))
  ;;       (emacs-version (emacs-version)))
  ;;   (format "%s\, Copyright © 1996-%s, Free Software Foundation, Inc." emacs-version current-year)))

(defun my-Ddays ()
  "Calculate the number of days until 2024-12-31."
  (let* ((today (current-time))
         (target-date (encode-time 0 0 0 31 12 2024))
         (diff-days (floor (/ (float-time (time-subtract target-date today)) 86400))))
    (format "/ D-day %d日前" diff-days)))

(defun my-todays-pop ()
  "오늘의 정보. 일자, 일정 등"
  (interactive)
  (let ((buffer (get-buffer-create "Today's Happy"))
        (current-date (format-time-string "● 오늘 %Y-%m-%d (%A) "))
        (d-day (my-Ddays))  ;; D-day 계산
        (agenda-string (with-temp-buffer
                         (org-agenda-list 3)   ;; 3일 간의 일정 가져오기
                         (goto-char (point-min)) ;; 버퍼의 맨 처음으로 이동
                         (forward-line 1)        ;; 첫 번째 줄을 건너뜀
			 (replace-regexp-in-string "^" "- " (buffer-substring-no-properties (point) (point-max)))))
;;                         (buffer-substring-no-properties (point) (point-max))))  ;; 나머지 일정을 문자열로 변환
        (random-quote (get-random-quote-from-creading))  ;; 무작위 인용구 가져오기
        (left-margin "    ")                    ;; 왼쪽 여백을 위한 변수
        (quote-margin "        "))              ;; 왼쪽 여백을 위한 변수(인용문)
    (with-current-buffer buffer
      (erase-buffer)                             ;; 기존 내용을 지우고
      (fancy-splash-head)                        ;; 기본 로고 출력
      (insert quote-margin (my-emacs-copyright)) ;; Copyright 출력
      (insert "\n")                              ;; 줄 바꿈 추가
      (insert left-margin current-date d-day)    ;; 날짜 삽입
      (insert (format "\n%s● 일정\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin agenda-string))  ;; 일정 삽입 (각 줄마다 여백 추가)
      (insert (format "\n%s● 인용구\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin random-quote))  ;; 인용구 삽입 (각 줄마다 여백 추가)
      (goto-char (point-min))                    ;; 커서를 버퍼의 맨 처음으로 이동
      (goto-line 2)
      (beginning-of-line))
    (switch-to-buffer buffer)))  ;; 버퍼 전환

(global-set-key (kbd "s-3") 'my-todays-pop)
