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
;;; Hunspell 설정
;; ======================================-
(use-package ispell
  :if my-mactop-p
  :commands (my-enable-korean-spell-check)
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "ko_KR")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8))))

  (defun my-enable-korean-spell-check ()
    "Enable Korean spell checking manually."
    (interactive)
    (setq ispell-local-dictionary "ko_KR")
    (flyspell-mode 1))

;;  (global-set-key (kbd "C-c s") 'my-enable-korean-spell-check))

;; =======================================
;;; gptel
;; ======================================-
(use-package gptel
  :ensure nil
  :if my-Macbook-p
  :bind ("C-c G" . gptel)
  :config
  (setq gptel-model "gpt-4-turbo")	;GPT-4o-mini
  (setq gptel-api-key
        (plist-get
         (car (auth-source-search :host "openai.com" :user "api_key"))
         :secret)))

;; =======================================
;;; fancy-startup-screen
;; ======================================-
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
          (let* ((quote (nth (random (length quotes)) quotes))
                 (formatted-quote (format "▷ %s\n\n%s" (car quote) (cdr quote))))
            formatted-quote)
        "No quotes found in cReading.org"))))

(defun my-fancy-startup-screen ()
  "Customized startup screen with default logo and modified text in a new buffer."
  (let ((buffer-name "*Startup Screen*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t)
            (left-margin "    "))  ;; 4칸 공백 정의
        (erase-buffer)  ;; 현재 버퍼의 내용을 지움
        (fancy-splash-head)	     ;기본 로고 
        ;; 로고 아래에 표시할 메시지 추가
        (insert (concat left-margin "Welcome to GNU Emacs, Copyright © 1996-2024 Free Software Foundation, Inc..\n\n"))
        ;; 랜덤 인용문에 여백 추가
        (let ((quote (get-random-quote-from-creading)))
          (insert (replace-regexp-in-string "^" left-margin quote))))
      ;; 위에서 4번째 줄로 커서 이동
      (goto-line 4)
      (beginning-of-line)
      (switch-to-buffer buffer-name))))


;; end here
(provide 'my-useful-custom)
