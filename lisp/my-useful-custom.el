;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el


;;; ###autoload
;; (defun my-today-stamp ()
;;   "Prompt for a date format and insert it at point."
;;   (interactive)
;;   (let* ((formats `(("ISO (YYYY-MM-DD)"       . "%Y-%m-%d")
;;                     ("Dot (YYYY.MM.DD)"       . "%Y.%m.%d")
;;                     ("DateTime (ISO + Time)"  . "%Y-%m-%d %R")
;;                     ("Weekday (ISO + Day)"    . ,(lambda () 
;;                                                    (format-time-string "%Y-%m-%d %A")))))
;;          (choice (completing-read "Select date format: " (mapcar #'car formats) nil t))
;;          (action (cdr (assoc choice formats))))
;;     (when action
;;       (if (functionp action)
;;           (insert (funcall action))
;;         (insert (format-time-string action))))))


;;; ###autoload
(defun my-select-current-line ()
 "Select the entire current line as an active region."
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))


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


(defun my/deactivate-input-method (&rest _args)
  "Deactivate current input method."
  (when (and (boundp 'current-input-method) current-input-method)
    (deactivate-input-method)))


;;; ###autoload
;; (defun-open-in-finder ()
;;   "Open current file in Finder"
;;   (interactive)
;;   (shell-command (concat "open -R " (shell-quote-argument buffer-file-name))))


;;https://github.com/protesilaos/dotfiles
;;;###autoload
(defun my-simple-indent-dwim ()
  "Indent the current defun in `prog-mode' or paragraph in `text-mode'."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'prog-mode)
      (mark-defun))
     ((derived-mode-p 'text-mode)
      (mark-paragraph)))
    (indent-for-tab-command)
    (deactivate-mark)))


(defun my/emacs-copyright ()
  "Return Emacs copyright with current year."
  (format "Copyright © 1996-%s,  Free Software Foundation, Inc."
          (format-time-string "%Y")))


(defun my-open-pdf-with-external-app ()
  "Open PDF with macOS default app and close buffer."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if (and file-path (file-exists-p file-path))
        (progn
          (start-process "pdf-external-open" nil "open" file-path)
          (kill-buffer (current-buffer)))
      (message "Error: Invalid file path or file does not exist."))))


(defun my-keyboard-quit-dwim ()
  "Do-what-I-mean quit behavior.
Handle 'keyboard-quit' based on the current context, such as an active region, open minibuffer, or the Completions buffer."
  (interactive)
  (cond
   ((region-active-p) ; 1. 블록이 잡혀있으면 블록 해제
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode) ; 2. 완성 목록창이 떠 있으면 닫기
    (delete-completion-window))
   ((> (minibuffer-depth) 0) ; 3. 미니버퍼가 열려있으면 (포커스 상관없이) 닫기
    (abort-recursive-edit))
   (t ; 4. 그 외에는 일반적인 Quit
    (keyboard-quit))))


(defun my-smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (call-interactively 'back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (call-interactively 'beginning-of-line))))


;;;###autoload
(defun my-toggle-window-split-ratio ()
  "Cycle the current window's width between 1/3, 2/3, and 1/2 of the frame."
  (interactive)
  (let* ((total-width (frame-width))
         (one-third (round (* total-width 0.33)))
         (one-half (round (* total-width 0.5)))
         (two-thirds (round (* total-width 0.66)))
         (current-width (window-total-width))

         (target-width (cond ((< current-width (* total-width 0.4)) two-thirds) ; 1/3 근처면 2/3로
                             ((< current-width (* total-width 0.6)) one-third)  ; 1/2 근처면 1/3로
                             (t one-half)))                                     ; 그 외(2/3)면 1/2로
         (delta (- target-width current-width)))
    (window-resize nil delta t)
    (message "Window width: %s" 
             (cond ((= target-width one-third) "1/3")
                   ((= target-width one-half) "1/2 (Balanced)")
                   (t "2/3")))))


;;;###autoload
(defun my-toggle-window-height-ratio ()
  "Toggle the current window's height between 1/3 and 2/3 of the frame.
This function preserves all buffer contents and works regardless of 
the number of open windows. It only adjusts the window's boundary."
  (interactive)
  (let* ((total-height (frame-height))
         (one-third (round (* total-height 0.33)))
         (two-thirds (round (* total-height 0.66)))
         (current-height (window-total-height))

         (target-height (if (< (abs (- current-height one-third)) 
                              (abs (- current-height two-thirds)))
                           two-thirds
                         one-third))
         (delta (- target-height current-height)))
    ;; window-resize의 세 번째 인자가 nil이면 세로(높이) 조절입니다.
    (window-resize nil delta nil)
    (message "Window height toggled to approx %s" 
             (if (= target-height one-third) "1/3" "2/3"))))


;;;###autoload
(defun my-toggle-window-dedicated ()
  "Toggle whether the current window is dedicated to its current buffer.
A dedicated window will not be used by Emacs to display other buffers."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p)))
  (message "Window is %s dedicated" 
           (if (window-dedicated-p) "NOW" "NO LONGER")))



;; (defun my-Ddays ()
;;   "Calculate days until/since 2024-12-31."
;;   (let ((diff-days (floor (/ (float-time (time-subtract (current-time)
;;                                                         (encode-time 0 0 0 16 12 2025)))
;; ;;                                                      (encode-time 0 0 0 31 12 2024)))
;;                              86400))))
;;     (if (> diff-days 0)
;;         (format "/  %d일 경과" diff-days)
;;       (format "/ D-day %d일前" (- diff-days)))))




(provide 'my-useful-custom)
;;; my-useful-custom.el ends here
