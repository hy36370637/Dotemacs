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
;; https://github.com/protesilaos
(defun my-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.  For
  recursive minibuffers, make sure to only close one level of depth.
- When in a *Completions* or `special-mode' buffer (e.g. *Help* or
  *Messages*), close it.
- When the *Completions* or a `special-mode' buffer is on display but
  not selected, close it.  If there are more than one, close them all.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((and (derived-mode-p 'completion-list-mode 'special-mode)
         (not (one-window-p)))
    (quit-window))
   ((when-let* ((_ (not (one-window-p)))
                (windows (seq-filter
                          (lambda (window)
                            (with-selected-window window
                              (derived-mode-p 'completion-list-mode 'special-mode)))
                          (window-list))))
      (dolist (window windows)
        (quit-window nil window))))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(provide 'my-useful-custom)
;;; my-useful-custom.el ends here
