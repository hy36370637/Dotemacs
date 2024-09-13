;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
(defun select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((choi '("·"  "→"  "⇒"  "「」"  "『』"  "※"  "…"  "―")))
    (insert (completing-read "선택: " choi))))

;; =======================================
;;; flush-line
;; ======================================-
;; M-x flush-lines RET ^\s-*$ RET 
(defun my/flush-empty-lines-in-region (start end)
  "Delete all empty or whitespace-only lines in the region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end))

;; 지정된 범위 라인 끝 공백 제거
(defun my/delete-trailing-whitespace-region (start end)
  "줄 끝의 공백과 탭을 제거,  줄바꿈과 빈 줄 그대로 유지."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "")))))

(defun my/delete-trailling-whitespace-line-join (&optional start end)
  "줄끝의 줄바꿈을 공백으로 대체하고 줄(라인) 연결, 연속된 빈 줄을 하나로 줄여 유지"
 (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward "\\([^\n]\\)\\(\n\\)\\([^\n]\\)" nil t)
          (replace-match "\\1 \\3"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n+" nil t)
          (replace-match "\n\n"))))))

;; ======================================
;;; random-quote
;; ======================================
(defun get-random-quote-from-creading ()
  (with-temp-buffer
    (insert-file-contents "~/Dropbox/Docs/Person/cReading.org")
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
                 (formatted-quote (format "%s\n\n%s" (car quote) (cdr quote))))
            formatted-quote)
        "No quotes found in cReading.org"))))

(defun my-display-random-quote-newtab ()
  "Display a random quote from cReading.org in a new tab."
  (interactive)
  (let* ((quote-text (get-random-quote-from-creading))
         (quote-buffer (get-buffer-create "*Random Quote*")))
    (with-current-buffer quote-buffer
      (erase-buffer)
      (insert quote-text)
      (goto-char (point-min))
      (let ((fill-column 140))  ; Set a reasonable line width
        (fill-region (point-min) (point-max)))
      (read-only-mode 1)  ; Make the buffer read-only
      (local-set-key (kbd "q") 'kill-current-buffer))
    (tab-bar-mode 1)  ; Ensure tab-bar-mode is enabled
    (tab-bar-new-tab)
    (tab-bar-rename-tab "Random Quote")
    (switch-to-buffer quote-buffer)))

;; ;; Add this function to automatically display a quote on Emacs startup
;; (defun auto-display-random-quote ()
;;   "Automatically display a random quote when Emacs starts."
;;   (run-with-timer 1 nil 'my-display-random-quote-newtab))

;; ;; Add the auto-display function to the after-init-hook
;; (add-hook 'after-init-hook 'auto-display-random-quote)



;; end here
(provide my-useful-custom)
