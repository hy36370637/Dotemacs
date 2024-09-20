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
;;; flush-line
;; ======================================-
;; M-x flush-lines RET ^\s-*$ RET 
;; (defun my/flush-empty-lines-in-region (start end)
;;   "Delete all empty or whitespace-only lines in the region."
;;   (interactive "r")
;;   (flush-lines "^\\s-*$" start end))

;; 지정된 범위 라인 끝 공백 제거
(defun my/delete-trailing-whitespace-region (start end)
  "줄 끝의 공백과 탭을 제거,  줄바꿈과 빈 줄 그대로 유지."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "")))))

(defun my/delete-trailing-whitespace-line-join (&optional start end)
  "줄끝의 줄바꿈을 공백으로 대체하고 줄(라인) 연결, 연속된 빈 줄을 하나로 줄여 유지"
 (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward "\\([^\n]\\)\\(\n\\)\\([^\n]\\)" nil t)
          (replace-match "\\1 \\3"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n+" nil t)
          (replace-match "\n\n"))))))

;; ======================================
;;; random-quote
;; ======================================
(defun get-random-quote-from-creading ()
  (with-temp-buffer
    (insert-file-contents "~/Dropbox/Docs/Person/cReading.org")
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
                 (formatted-quote (format "%s\n\n%s" (car quote) (cdr quote))))
            formatted-quote)
        "No quotes found in cReading.org"))))

(defun my-display-random-quote-newtab ()
  "Display a random quote from cReading.org in a new tab."
  (interactive)
  (let* ((quote-text (get-random-quote-from-creading))
         (quote-buffer (get-buffer-create "*Random Quote*")))
    (with-current-buffer quote-buffer
      (erase-buffer)
      (insert quote-text)
      (goto-char (point-min))
      (let ((fill-column 140))  ; Set a reasonable line width
        (fill-region (point-min) (point-max)))
      (read-only-mode 1)  ; Make the buffer read-only
      (local-set-key (kbd "q") 'kill-current-buffer))
    (tab-bar-mode 1)  ; Ensure tab-bar-mode is enabled
    (tab-bar-new-tab)
    (tab-bar-rename-tab "Random Quote")
    (switch-to-buffer quote-buffer)))

;; ;; Add this function to automatically display a quote on Emacs startup
;; (defun auto-display-random-quote ()
;;   "Automatically display a random quote when Emacs starts."
;;   (run-with-timer 1 nil 'my-display-random-quote-newtab))

;; ;; Add the auto-display function to the after-init-hook
;; (add-hook 'after-init-hook 'auto-display-random-quote)

(use-package hl-line
  :ensure nil
  :custom
  (hl-line-sticky-flag nil)
  :hook
  (dired-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (emacs-lisp-mode . hl-line-mode))

;;;; pdf-viewer
;; 페이지에 맞추기: 0,  너비에 맞추기: W,  높이에 맞추기: H
;; 아웃라인 열기: o,  아웃라인에서 항목 선택: RET
;; 텍스트 주석 추가: C-c C-a t,  하이라이트 주석 추가: C-c C-a h,  주석 목록 보기: C-c C-a l
;; 메타데이터 보기: D,  90도 회전: R,  슬라이스 모드 (여백 자르기): s s
;; PDF에서 소스로 이동: C-c C-g,  소스에서 PDF로 이동: C-c C-g (AUCTeX에서)
(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)
;;  (setq-default pdf-view-display-size 'fit-page)  ;default  'fit-width
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-continuous t)
  (setq pdf-outline-imenu-use-flat-menus t)
  (setq pdf-view-max-image-width 2048)
  (setq pdf-annot-default-annotation-properties
      '((highlight
         (color . "yellow"))
        (underline
         (color . "blue"))
        (squiggly
         (color . "orange"))
        (strike-out
         (color . "red"))
        (text
         (color . "blue")
         (icon . "note"))))
  ;; 야간 모드 색상 설정 (수동으로 전환 가능)
  (setq pdf-view-midnight-colors '("#839496" . "#002b36"))
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :bind (:map pdf-view-mode-map
              ;; 네비게이션
              ("C-n" . pdf-view-next-page-command)
              ("C-p" . pdf-view-previous-page-command)
              ("n" . next-line)
              ("p" . previous-line)
              ("g" . pdf-view-first-page)
              ("G" . pdf-view-last-page)
              ;; 줌
              ("+" . pdf-view-enlarge)
              ("-" . pdf-view-shrink)
              ("0" . pdf-view-scale-reset)
              ;; 검색
              ("C-s" . isearch-forward)
              ("/" . isearch-forward)
              ("C-r" . isearch-backward)
              ;; 주석
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("u" . pdf-annot-add-underline-markup-annotation)
              ("d" . pdf-annot-add-squiggly-markup-annotation)
              ("s" . pdf-annot-add-strikeout-markup-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("D" . pdf-annot-delete)
              ;; 기타
              ("C-c n" . pdf-view-midnight-minor-mode)  ; 야간 모드 토글
              ("C-c t" . pdf-view-themed-minor-mode)    ; 테마 모드 토글
              ("C-c i" . pdf-view-auto-slice-minor-mode) ; 자동 슬라이스 모드
              ("C-c o" . pdf-outline))   
  :mode ("\\.pdf\\'" . pdf-view-mode))




;; end here
(provide 'my-useful-custom)
