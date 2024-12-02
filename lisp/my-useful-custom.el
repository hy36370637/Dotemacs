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
(global-set-key (kbd "C-c SPC C") 'select-special-character)

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
  (setq gptel-model "o1-preview")	;GPT-4o-mini
;;  (setq gptel-model "gpt-4-turbo")	;GPT-4o-mini
  (setq gptel-api-key
        (plist-get
         (car (auth-source-search :host "openai.com" :user "api_key"))
         :secret)))

;; =======================================
;;; writerroom
;; ======================================-
(use-package writeroom-mode
  :ensure nil
;;  :hook (org-mode . writeroom-mode) ; Org mode에서 writeroom-mode 자동 활성화
  :init
  (setq writeroom-width 100)                    ; 글쓰기 너비 설정
;;  (setq writeroom-mode-line t)                  ; 모드라인 표시 여부
;;  (setq writeroom-global-effects '(writeroom-toggle-fullscreen
;;                                   writeroom-toggle-alpha)) ; 전역 효과 설정
  :bind ("C-c w" . writeroom-mode))             ; writeroom-mode 토글 단축키

;; =======================================
;;; spacious-padding
;; ======================================-
;; (use-package spacious-padding
;;   :ensure nil
;;   :if (display-graphic-p)
;;   :bind ("C-c SPC P" . spacious-padding-mode)
;;   :init
;;   (setq spacious-padding-widths
;;         '( :left-fringe-width 20
;;            :right-fringe-width 20)))

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
