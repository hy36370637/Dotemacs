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




;; end here
(provide 'my-useful-custom)
