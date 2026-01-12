;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el

;;; ###autoload
(defun my-today-stamp ()
  "다양한 today 포맷 선택 삽입."
  (interactive)
  (let* ((formats `(("YYYY-MM-DD"       . "%Y-%m-%d")
                    ("YYYY.MM.DD"       . "%Y.%m.%d")
                    ("YYYY-MM-DD HH:MM" . "%Y-%m-%d %R")
                    ("YYYY-MM-DD 요일"  . ,(lambda () 
                                           (format-time-string "%Y-%m-%d %A")))))
         (choice (completing-read "날짜 포맷 선택: " (mapcar #'car formats) nil t))
         (action (cdr (assoc choice formats))))
    (when action
      (if (functionp action)
          (insert (funcall action))
        (insert (format-time-string action))))))

;;; ###autoload
(defun my-select-current-line ()
  "현재 줄 전체 선택."
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

;;; ###autoload
(defun my-newline ()
  "현재 줄 아래, 새로운 줄 만들기."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;; ###autoload
(defun my-newline-above ()
  "현재 줄 위에, 새로운 줄 만들기."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))



(provide 'my-useful-custom)
;;; my-useful-custom.el ends here
