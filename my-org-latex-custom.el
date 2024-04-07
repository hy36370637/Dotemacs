;; ======================================
;;; my-latex-custom-function/for org-mode
;; --------------------------------------
;; text-color change
;; org-mode에서 PDF 생성
;; emacs/lisp/my-org-latex-custom.el
(defun latex-text-color (text color)
  "Return LaTeX text with specified color."
  (format "\\textcolor{%s}{%s}" color text))

(defun insert-latex-text-color (begin end)
  "latex. selected-text color change"
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties begin end))
            (color (read-string "Enter color: ")))
        (delete-region begin end)
        (insert (latex-text-color selected-text color)))
    (message "No region selected")))

(defun latex-modify-text (begin end modifier)
  "Modify selected text, '_ for subscript and '^ for superscript."
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties begin end)))
        (delete-region begin end)
        (setq selected-text (concat modifier "{" selected-text "}"))
        (insert selected-text))
    (message "No region selected")))

;; 단락으로 분명하게 구분된 영역만 사용
;; C-c C-, org-insert-structure-template 사용 권장
(defun my-latex-insert-block () ;; ver 0.2.1
  "Inserts `#+begin_block`, `#+end_block`. selected region."
  (let ((block-type (completing-read "Choose block type: " '("quote" "verse"))))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end))
              (indent ""))
          ;; Determine the current indentation level
          (save-excursion
            (goto-char beg)
            (skip-chars-forward "[:space:]")
            (setq indent (concat (make-string (current-column) ?\s))))
          (save-excursion
            (goto-char beg)
            (insert (format "%s#+begin_%s\n" indent block-type))
            ;; Apply the same indentation to the end of the block
            (goto-char end)
  ;;          (forward-line)
            (end-of-line)
            (insert-before-markers (format "\n%s#+end_%s" indent block-type))))
      (message "No region selected"))))

;;; 통합본
(defun my-org-latex-custom(begin end)
  "Custom function for Org export LaTeX. Text color, sub-Super Script, quote-verse Block"
  (interactive "r")
  (if (use-region-p)
      (let ((choice (read-char-choice "Select action: [c]글자색, [s]아래첨자, [S]위첨자, [b]블록: " '(?c ?s ?S ?b))))
        (pcase choice
          (?c (insert-latex-text-color begin end))
          (?s (latex-modify-text begin end "_"))
          (?S (latex-modify-text begin end "^"))
          (?b (my-latex-insert-block))))
    (message "No region selected")))

(provide 'my-org-latex-custom)
