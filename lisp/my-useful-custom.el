;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
;;;###autoload
(defun my-insert-special-character ()
  "특수 문자 삽입. 괄호 쌍 삽입 시 TAB 키로 괄호를 탈출."
  (interactive)
  (let* ((chars '("·" "→" "⇒" "※" "…" "―" "《》" "「」" "『』"))
         (choice (completing-read "특수 문자 선택: " chars nil t)))
    (unless (string-empty-p choice)
      (if (= (length choice) 2)
          (progn
            (insert (substring choice 0 1) (substring choice 1 2))
            (backward-char 1)
            (my--enable-tab-escape))
        (insert choice)))))

;;;###autoload
(defun my-region-dblspecial-characters ()
  "선택 영역 또는 커서 위치의 단어를 괄호로 감쌉니다."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (start (car bounds))
         (end (cdr bounds))
         (brackets '(("<left>"   . ("「" . "」"))
                     ("<right>"  . ("『" . "』"))
                     ("<up>"     . ("'" . "'"))
                     ("<down>"   . ("\"" . "\""))
                     ("M-<left>" . ("《" . "》"))))
         (key (read-key-sequence "괄호: [←]「」 [→]『』 [↑]'' [↓]\"\" [M-←]《》"))
         (pair (alist-get (key-description key) brackets nil nil #'string=)))
    (when pair
      (save-excursion
        (goto-char end) (insert (cdr pair))
        (goto-char start) (insert (car pair)))
      (goto-char (1+ start))
      (my--enable-tab-escape))))

(defun my--enable-tab-escape ()
  "TAB 키로 한 칸 이동하는 일회성 키맵 활성화."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "TAB")
                 (lambda () (interactive) (forward-char 1)))
     map)
   t
   (lambda () (message "괄호 탈출!"))))

(global-set-key (kbd "C-c j c") 'my-insert-special-character)
(global-set-key (kbd "C-c j r") 'my-region-dblspecial-characters)

;; =======================================
;;; Hunspell 설정
;; ======================================-
(use-package ispell
  :if my-macOS-p
;;  :hook (text-mode . my/korean-spell-check)
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8)))

  (defun my-korean-spell-check ()
    "Enable Korean spell checking for the current buffer."
    (interactive)
    (setq-local ispell-local-dictionary "ko_KR")
    (flyspell-mode 1)))
(global-set-key (kbd "C-c j h") 'my-korean-spell-check)

;; ======================================
;;; Date,Time insert
;; ======================================
(defun my-today-stamp ()
  "다양한 today 포맷 선택 삽입."
  (interactive)
  (let* ((formats `(("YYYY-MM-DD"       . "%Y-%m-%d")
                    ("YYYY.MM.DD"       . "%Y.%m.%d")
                    ("YYYY-MM-DD HH:MM" . "%Y-%m-%d %R")
                    ("YYYY-MM-DD 요일"  . ,(lambda () 
                                         (format-time-string "%Y-%m-%d %A")))))
         (choice (completing-read "날짜 포맷 선택: " (mapcar #'car formats) nil t))
         (action (cdr (assoc choice formats))))
    (when action
      (if (functionp action)
          (insert (funcall action))
        (insert (format-time-string action))))))

;; =======================================
;;; gptel
;; ======================================-
;; (use-package gptel
;;   :ensure t
;;   :if my-Macbook-p
;;   :defer t
;;   :commands (gptel gptel-send)
;;   :bind (("C-c n g" . gptel)          ; 전용 AI 채팅 버퍼 열기
;;          ("C-c n q" . gptel-send))   ; 선택 영역 혹은 현재 문맥 질문하기
;;   :config
;;   (setq gptel-api-key
;;         (funcall
;;          (plist-get
;;           (car (auth-source-search :host "gemini.google.com"
;;                                    :user "api_key"))
;;           :secret)))
;;   (gptel-make-gemini "Gemini"
;;     :key gptel-api-key
;;     :stream t                         ; 답변을 실시간 스트리밍으로 출력
;;     :models '("gemini-2.0-flash-001"  ; Gemini 3의 기반이 되는 최신 Flash 모델
;;               "gemini-2.0-pro-exp"    ; 최신 고성능 추론 모델
;;               "gemini-1.5-pro"))      ; 안정적인 긴 문맥 처리용
;;   (setq gptel-backend (gptel-get-backend "Gemini")
;;         gptel-model "gemini-2.0-flash-001"  ; 가장 빠르고 스마트한 기본값
;;         gptel-default-mode 'org-mode)       ; Org-mode 기반으로 AI와 대화
;;   (setq gptel-directives
;;         '((default . "당신은 Gemini 3 기반의 유능한 AI 파트너입니다. 한국어로 답변하세요.")
;;           (coder . "Emacs Lisp 전문가로서 간결한 코드와 명확한 주석을 제공하세요.")
;;           (editor . "Org-mode 문서의 논리를 분석하고 문체와 가독성을 개선하세요."))))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :defer t
  :commands (magit-status magit-log magit-blame)
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))



;; end here
(provide 'my-useful-custom)
