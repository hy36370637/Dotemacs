;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
;;inspire https://protesilaos.com
(defcustom my-pair-pairs
  '((?* :description "Bold"           :pair ?*)
    (?/ :description "Italic"         :pair ?/)
    (?= :description "Verbatim"       :pair ?=)
    (?~ :description "Code"           :pair ?~)
    (?+ :description "Strike"         :pair ?+)
    (?\" :description "Double Quotes" :pair ?\")
    (?\' :description "Single Quotes" :pair ?\')
    (?\( :description " () "          :pair (?\( . ?\)))
    (?\[ :description " [] "          :pair (?\[ . ?\]))
    (?{  :description " {} "          :pair (?{ . ?}))
    (?<  :description "「」"          :pair ("「" . "」"))
    (?>  :description "『』"          :pair ("『" . "』")))
;;    (?M  :description "《》"          :pair ("《" . "》")))
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
  (interactive "c기호 입력 (*, /, =, ~, (, [, <, > ...): ")
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
      (message "'%s' 완료 (TAB으로 탈출)" (plist-get (cdr entry) :description)))))

;; =======================================
;;; Hunspell 설정
;; ======================================-
(defun my-korean-spell-check ()
  "Flyspell 모드를 한국어 사전과 함께 활성화합니다."
  (interactive)
  (require 'ispell) ;; 함수 실행 시 패키지 로드
  (setq ispell-local-dictionary "ko_KR")
  (flyspell-mode 1)
  (message "Korean spell check enable"))

(use-package ispell
  :if my-macOS-p
  :defer t
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8)))
  (setq flyspell-delay 0.5)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-use-meta-tab nil))

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

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :defer t
;;  :commands (magit-status magit-log magit-blame)
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))



;; end here
(provide 'my-useful-custom)
