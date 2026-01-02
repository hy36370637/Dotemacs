;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;

;; ======================================
;;; Date,Time insert
;; ======================================
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



;; end here
(provide 'my-useful-custom)
