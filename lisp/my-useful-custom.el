;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
(defcustom my-pair-pairs
  '((?* :description "Bold"           :pair ?*)
    (?/ :description "Italic"         :pair ?/)
    (?= :description "Verbatim"       :pair ?=)
    (?~ :description "Code"           :pair ?~)
    (?+ :description "Strike"         :pair ?+)
    (?\" :description "Double Quotes" :pair ?\")
    (?\' :description "Single Quotes" :pair ?\')
    (?\( :description "Parentheses"   :pair (?\( . ?\)))
    (?\[ :description "Square brackets" :pair (?\[ . ?\]))
    (?{  :description "Curly brackets"  :pair (?{ . ?}))
    (?<  :description "「」"           :pair ("「" . "」"))
    (?>  :description "『』"           :pair ("『" . "』"))
    (?M  :description "《》"           :pair ("《" . "》")))
  "Org-mode 마커 및 특수 괄호 쌍 리스트"
  :group 'editing
  :type '(alist :key-type character :value-type (plist)))

(defun my--enable-tab-escape ()
  "기호 삽입 후 TAB 키로 한 칸 이동하는 일회성 키맵 활성화."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "TAB")
                 (lambda () (interactive) (forward-char 1)))
     map)
   t))

(defun my-region-wrap (char)
  "선택 영역이나 단어를 입력받은 CHAR 쌍으로 감쌉니다. TAB으로 탈출 가능합니다."
  (interactive "c기호 입력 (*, /, =, ~, <, >, M ...): ")
  (let* ((entry (assoc char my-pair-pairs))
         (pair-data (plist-get (cdr entry) :pair))
         (open (if (consp pair-data) (car pair-data) pair-data))
         (close (if (consp pair-data) (cdr pair-data) pair-data))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
         (start (car bounds))
         (end (cdr bounds)))

    (if (not pair-data)
        (message "정의되지 않은 기호: %c" char)
      (save-excursion
        ;; 닫는 기호 삽입
        (goto-char end)
        (insert (if (characterp close) (char-to-string close) close))
        ;; 여는 기호 삽입
        (goto-char start)
        (insert (if (characterp open) (char-to-string open) open)))
      
      ;; 커서 위치 조정 및 TAB 탈출 기능 활성화
      (unless (use-region-p) (goto-char (1+ end)))
      (my--enable-tab-escape)
      (message "'%s' 적용 완료 (TAB으로 탈출)" (plist-get (cdr entry) :description)))))

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
;; (global-set-key (kbd "C-c j h") 'my-korean-spell-check)

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
