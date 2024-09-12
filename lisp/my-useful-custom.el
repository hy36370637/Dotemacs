;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
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
