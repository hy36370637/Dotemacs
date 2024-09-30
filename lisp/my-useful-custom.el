;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
(defun select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((choi '("·"  "→"  "⇒"  "「」"  "『』"  "※"  "…"  "―" "《》")))
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

(defun my-emacs-copyright ()
  "Return the copyright information for Emacs, including the current year."
  (let ((current-year (format-time-string "%Y")))
    (format "Copyright © 1996-%s Free Software Foundation, Inc." current-year)))

(defun my-fancy-startup-screen ()
  "Customized startup screen with default logo and randon quote in a new buffer."
  (let ((buffer-name "*Startup Screen*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t)
            (left-margin "    "))       ; 4칸 공백 정의
        (erase-buffer)                   ; 현재 버퍼 내용 지움
        (fancy-splash-head)	     ;기본 로고 
        ;; 로고 아래에 표시할 메시지
	(insert (concat left-margin 
                (format "💕 Welcome to GNU Emacs %s, %s.\n\n"
                        emacs-version
                        (my-emacs-copyright))))
        ;; 랜덤 인용문에 여백 추가
        (let ((quote (get-random-quote-from-creading)))
          (insert (replace-regexp-in-string "^" left-margin quote))))
      ;; line break
      (goto-char (point-min))
      (let ((fill-column 110))  ; Set a reasonable line width
        (fill-region (point-min) (point-max)))
      ;; 위에서 2번째 줄로 커서 이동
      (goto-line 2)
      (beginning-of-line)
      (switch-to-buffer buffer-name))))

;; =======================================
;;; spacious-padding
;; ======================================-
(use-package spacious-padding
  :ensure nil
  :if (display-graphic-p)
  :bind ("C-x c p" . spacious-padding-mode)
  :init
  (setq spacious-padding-widths
        '( :left-fringe-width 20
           :right-fringe-width 20)))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))

;; ======================================
;;; Move to beginning of line
;; ======================================
;; from https://sachachua.com/dotemacs/index.html#move-to-beginning-of-line
(defun my-smarter-move-beginning-of-line (arg)
  "줄의 시작 부분 문자로 이동(공백 제외)
      ARG가 nil이 아니거나 1이 아니면, 먼저 ARG - 1 줄 앞으로 이동. 포인트가 버퍼의 시작이나 끝에 도달하면 그곳에서 멈춤."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my-smarter-move-beginning-of-line)




;; end here
(provide 'my-useful-custom)
