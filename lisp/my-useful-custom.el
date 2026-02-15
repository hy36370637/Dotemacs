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
  "Toggle the current window's width between 1/3 and 2/3 of the frame.
Does not include 1/2 ratio; use `balance-windows' (C-x +) for equal splits.
Preserves all buffer contents during the resize."
  (interactive)
  (let* ((total-width (frame-width))
         (current-width (window-total-width))
         ;; 현재 비율이 50%보다 작으면 2/3로, 크면 1/3로 목표 설정
         (target-width (if (< (/ (float current-width) total-width) 0.5)
                           (round (* total-width 0.66))
                         (round (* total-width 0.33))))
         (delta (- target-width current-width)))
    (window-resize nil delta t)
    (message "Window width toggled (1/3 <-> 2/3)")))


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
         ;; 현재 높이가 1/3에 가까우면 2/3로, 아니면 1/3로 목표 설정
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


;;;###autoload
(defun my-layout-3-windows-center-focus ()
  "Set a 25% | 50% | 25% layout for 3 windows, regardless of cursor position.
Windows are sorted by their horizontal position on the frame."
  (interactive)
  (let ((windows (window-list)))
    (if (= (length windows) 3)
        ;; 창들을 왼쪽 좌표(edges) 기준으로 정렬
        (let* ((sorted-windows (sort windows (lambda (w1 w2)
                                               (< (car (window-edges w1))
                                                  (car (window-edges w2))))))
               (total-width (frame-width))
               ;; (side-width (round (* total-width 0.25)))
               ;; (center-width (- total-width (* side-width 2)))
	       (side-width (round (* total-width 0.3)))
               (center-width (- total-width (* side-width 2)))
               (win-left (nth 0 sorted-windows))
               (win-center (nth 1 sorted-windows))
               (win-right (nth 2 sorted-windows)))
          
          ;; 1. 왼쪽 창 크기 고정
          (window-resize win-left (- side-width (window-total-width win-left)) t)
          ;; 2. 가운데 창 크기 조절 (나머지는 오른쪽 창이 됨)
          (window-resize win-center (- center-width (window-total-width win-center)) t)
          
          (message "Layout fixed: 25%% | 50%% | 25%% (Sorted by position)"))
      (message "Requires exactly 3 windows (current: %d)." (length windows)))))


;;;###autoload
(defun my-split-window-three-column ()
  "Split the current window into three columns with 25:50:25 ratio.
If more than one window exists, it will first delete other windows."
  (interactive)
  (delete-other-windows)
  ;; 1. 일단 3개로 분할
  (split-window-right)
  (split-window-right)
  ;; 2. 이전에 만든 25:50:25 레이아웃 함수 호출
  (my-layout-3-windows-center-focus)
  (message "Three-column layout initialized."))



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
