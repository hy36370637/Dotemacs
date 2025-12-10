;;; my-todays-pop.el --- Today's information display -*- lexical-binding: t; -*-
;; Author: Ho Young <hy36370637@gmail.com>

;;; Commentary:
;; Display today's date, agenda, and random quotes from cReading.org

;;; Code:

;; ======================================
;;; Helper Functions
;; ======================================

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
          (let ((quote (nth (random (length quotes)) quotes)))
            (format "%s\n%s" (car quote) (cdr quote)))
        "No quotes found in cReading.org"))))

(defun my-emacs-copyright ()
  "Return the copyright information for Emacs with current year."
  (format "Copyright © 1996-%s,  Free Software Foundation, Inc."
          (format-time-string "%Y")))

(defun my-Ddays ()
  "Calculate days remaining until or elapsed since 2024-12-31."
  (let* ((today (current-time))
         (target-date (encode-time 0 0 0 31 12 2024))
         (diff-seconds (float-time (time-subtract today target-date)))
         (diff-days (floor (/ diff-seconds 86400))))
    (if (> diff-days 0)
        (format "/  %d日 경과" diff-days)
      (format "/ D-day %d日前" (- diff-days)))))

(defun my-format-agenda-string ()
  "Get formatted 3-day agenda string with bullet points."
  (with-temp-buffer
    (org-agenda-list 3)
    (goto-char (point-min))
    (forward-line 1)
    (replace-regexp-in-string 
     "^" "- " 
     (buffer-substring-no-properties (point) (point-max)))))

;; ======================================
;;; Main Function
;; ======================================

(defun my-todays-pop ()
  "Display today's date, agenda, and random quote in a popup buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "Today's Happy"))
         (current-date (format-time-string "● 오늘 %Y-%m-%d (%A) "))
         (d-day (my-Ddays))
         (agenda-string (my-format-agenda-string))
         (random-quote (get-random-quote-from-creading))
         (left-margin "    ")
         (quote-margin "        "))
    
    (with-current-buffer buffer
      (erase-buffer)
      
      ;; Header
      (fancy-splash-head)
      (insert quote-margin (my-emacs-copyright) "\n")
      
      ;; Date
      (insert left-margin current-date d-day)
      
      ;; Agenda
      (insert (format "\n%s● 일정\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin agenda-string))
      
      ;; Quote
      (insert (format "\n%s● 글말\n" left-margin))
      (insert (replace-regexp-in-string "^" quote-margin random-quote))
      
      ;; Cursor position
      (goto-char (point-min))
      (forward-line 1)
      (beginning-of-line)
      
      ;; Key binding
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") 
          (lambda () (interactive) (kill-this-buffer)))
        (use-local-map map)))
    
    (switch-to-buffer buffer)))

;;; end here
(provide 'my-todays-pop)
