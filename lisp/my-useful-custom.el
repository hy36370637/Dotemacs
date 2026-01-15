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

;;; ###autoload
(defun my-select-line-left ()
  "Select the region from the current point to the beginning of the line."
  (interactive)
  (set-mark (line-beginning-position))
  (message "Selected to the beginning of the line."))

;;; ###autoload
(defun my-select-line-right ()
  "Select the region from the current point to the end of the line."
  (interactive)
  (set-mark (line-end-position))
  (message "Selected to the end of the line."))

;;; ###autoload
(defun my-duplicate-dwim ()
  "Duplicate the current region if active, otherwise duplicate the current line."
  (interactive)
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (filter-buffer-substring (line-beginning-position) (line-end-position)))))
        (goto-char (if use-region (region-end) (line-end-position)))
        (newline)
        (insert text)))
    ;; Move cursor to the next line for consecutive duplication
    (forward-line 1)))

;;; ###autoload
(defun my-query-replace-regexp-dwim (arg)
  "Replace in region if active, else in whole buffer."
  (interactive "P")
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (call-interactively #'query-replace-regexp))))




(provide 'my-useful-custom)
;;; my-useful-custom.el ends here
