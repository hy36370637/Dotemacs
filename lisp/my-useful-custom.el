;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-useful-custom.el
;;
;; =======================================
;;; 특수문자 입력
;; ======================================-
(defun my/select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((choi '("·"  "→"  "⇒"   "※"  "…"  "―" "《》")))
    (insert (completing-read "선택: " choi))))
(global-set-key (kbd "C-c k c") 'my/select-special-character)

(defun my/region-dblspecial-characters (start end)
  "Insert special characters around the selected region.
Use arrow keys to choose between 「」, 『』, '', and \"\".
Use option+left to select 《》."
  (interactive "r") ; Get the region's start and end
  (let* ((prompt "Choose brackets: [←]「」 [→]『』 [↑]'' [↓]\"\" [M-←]《》")
         (choice (catch 'done
                   (while t
                     (let ((key (read-key-sequence prompt)))
                       (cond
                        ((equal key (kbd "<left>")) (throw 'done "「」"))
                        ((equal key (kbd "<right>")) (throw 'done "『』"))
                        ((equal key (kbd "<up>")) (throw 'done "''"))
                        ((equal key (kbd "<down>")) (throw 'done "\"\""))
                        ((equal key (kbd "M-<left>")) (throw 'done "《》"))))))))
    (save-excursion
      (goto-char end)
      (insert (cond ((equal choice "「」") "」")
                    ((equal choice "『』") "』")
                    ((equal choice "''") "'")
                    ((equal choice "\"\"") "\"")
                    ((equal choice "《》") "》")))
      (goto-char start)
      (insert (cond ((equal choice "「」") "「")
                    ((equal choice "『』") "『")
                    ((equal choice "''") "'")
                    ((equal choice "\"\"") "\"")
                    ((equal choice "《》") "《"))))))

(global-set-key (kbd "C-c k C") 'my/region-dblspecial-characters)

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
;;    (setq ispell-local-dictionary "ko_KR")
    (flyspell-mode 1))

;; =======================================
;;; gptel
;; ======================================-
(use-package gptel
  :ensure nil
  :if my-Macbook-p
  :bind ("C-c G" . gptel)
  :config
;;  (setq gptel-model "o1-preview")	;GPT-4o-mini
  (setq gptel-model "gpt-4-turbo")	;GPT-4o-mini
  (setq gptel-api-key
        (plist-get
         (car (auth-source-search :host "openai.com" :user "api_key"))
         :secret)))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode t))

;; ======================================
;;; Date,Time insert
;; ======================================
(defun my/insert-today-stamp ()
  "Insert a date stamp based on user selection using arrow keys."
  (interactive)
  (let ((format (completing-read
                 "Choose date format (use arrow keys): "
                 '("YYYY-MM-DD" "YYYY.MM.DD" "YYYY-MM-DD HH:MM" "YYYY-MM-DD 요일")
                 nil t))) ; `nil t`는 기본 완성 기능을 활성화하고, 화살표 키로 선택 가능하게 함
    (cond
     ((string= format "YYYY-MM-DD")
      (insert (format-time-string "%Y-%m-%d")))
     ((string= format "YYYY.MM.DD")
      (insert (format-time-string "%Y.%m.%d")))
     ((string= format "YYYY-MM-DD HH:MM")
      (insert (format-time-string "%Y-%m-%d %R")))
     ((string= format "YYYY-MM-DD 요일")
      (let* ((weekday (format-time-string "%w"))
             (weekday-name (nth (string-to-number weekday)
                                '("일요일" "월요일" "화요일" "수요일" "목요일" "금요일" "토요일"))))
        (insert (format-time-string "%Y-%m-%d "))
        (insert weekday-name)))
     (t (message "Invalid format selection.")))))


;; end here
(provide 'my-useful-custom)
