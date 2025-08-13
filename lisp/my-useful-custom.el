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
(global-set-key (kbd "C-c p c") 'my/select-special-character)

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

(global-set-key (kbd "C-c p C") 'my/region-dblspecial-characters)

;; =======================================
;;; Hunspell 설정
;; ======================================-
(use-package ispell
  :if my-mactop-p
  :hook (text-mode . my-enable-korean-spell-check)
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8)))

  (defun my-enable-korean-spell-check ()
    "Enable Korean spell checking for the current buffer."
    (interactive)
    (setq-local ispell-local-dictionary "ko_KR")
    (flyspell-mode 1)))

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
(global-set-key (kbd "C-c p d") 'my/insert-today-stamp)

;; =======================================
;;; gptel
;; ======================================-
(use-package gptel
  :ensure t
  :if my-Macbook-p
  :bind ("C-c n g" . gptel)
  :config
  ;; API 키 불러오기
  (setq gptel-api-key
        (funcall
         (plist-get
          (car (auth-source-search :host "gemini.google.com"
                                   :user "api_key"))
          :secret)))

  ;; Gemini 백엔드 등록
  (gptel-make-gemini "Gemini"
    :key gptel-api-key
    :models '("gemini-2.5-flash" "gemini-2.5-pro"))

  ;; 기본 백엔드를 Gemini로 지정
  (setq gptel-backend (gptel-get-backend "Gemini"))
  (setq gptel-model "gemini-2.5-flash"))

;; ======================================
;;; Magit
;; ======================================
(use-package magit
  :bind (("C-c n m" . magit-status))
  :config
  (setq magit-auto-revert-mode t))

;; end here
(provide 'my-useful-custom)

;; ======================================
;;; pdf-tools TEST
;; ======================================
;; viewer 동작, search 한글 인코딩 문제(깨짐)
;; (use-package pdf-tools
;;   :ensure t
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   ;; epdfinfo 서버 경로 지정 (버전 번호 확인 필요)
;;   (setq pdf-info-epdfinfo-program
;;         (expand-file-name "~/.emacs.d/elpa/pdf-tools-20240429.407/epdfinfo"))

;;     (setq pdf-info-server-encoding-system 'utf-8-unix)
;;   ;; pdf-tools 초기화
;;   (pdf-tools-install) ;; 여기서 서버 빌드 & 로드

;;   ;; PDF 페이지를 화면에 맞게 표시
;; ;;  (setq-default pdf-view-display-size 'fit-page)
;;   (message "pdf-tools configuration loaded successfully."))
