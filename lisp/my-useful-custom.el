;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el


;;; ###autoload
(defun my-today-stamp ()
  "Prompt for a date format and insert it at point."
  (interactive)
  (let* ((formats `(("ISO (YYYY-MM-DD)"       . "%Y-%m-%d")
                    ("Dot (YYYY.MM.DD)"       . "%Y.%m.%d")
                    ("DateTime (ISO + Time)"  . "%Y-%m-%d %R")
                    ("Weekday (ISO + Day)"    . ,(lambda () 
                                                   (format-time-string "%Y-%m-%d %A")))))
         (choice (completing-read "Select date format: " (mapcar #'car formats) nil t))
         (action (cdr (assoc choice formats))))
    (when action
      (if (functionp action)
          (insert (funcall action))
        (insert (format-time-string action))))))


;;; ###autoload
(defun my-select-current-line ()
 "Select the entire current line as an active region."
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))


;;; ###autoload
(defun my-newline ()
  "Insert a new indented line below the current one."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


;;; ###autoload
(defun my-newline-above ()
  "Insert a new indented line above the current one."
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


(defun my-emacs-copyright ()
  "Return Emacs copyright with current year."
  (format "Copyright © 1996-%s,  Free Software Foundation, Inc."
          (format-time-string "%Y")))


(defun my-open-pdf-with-external-app ()
  "Open PDF files via macOS default application and terminate the Emacs buffer.
This ensures a seamless transition to the system viewer for reliable rendering."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if (and file-path (file-exists-p file-path))
        (progn
          (start-process "pdf-external-open" nil "open" file-path)
          (kill-buffer (current-buffer)))
      (message "Error: Invalid file path or file does not exist."))))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . my-open-pdf-with-external-app))



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
