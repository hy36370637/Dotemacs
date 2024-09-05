;; 완성completion 도움
;; .emacs.d/lisp/my-completion.el

;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure nil
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

;; ======================================
;;; marginalia
;; ======================================
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; ======================================
;;; orderless
;; ======================================
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ======================================
;;; consult
;; ======================================
(use-package consult
  :ensure t
  :bind
   ("C-x b" . consult-buffer)
  :config
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file))
  ;; 북마크 관련 추가 설정
  (setq consult-bookmark-narrow
        '((?b "Bookmarks" bookmark)
          (?f "Files" file)
          (?d "Directories" dir)))
  ;; 북마크 미리보기 설정
  (setq consult-preview-key 'any)
  ;; 북마크 정렬 설정
  (setq consult-sort-function #'consult--alpha-sort)
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; ======================================
;;; consult-dir
;; ======================================
(use-package consult-dir
  :ensure t)
  ;; :after vertico
  ;; :bind (("C-x c-d" . consult-dir)
  ;;         :map vertico-map
  ;;        ("C-x c-d" . consult-dir)
  ;; 	 ("C-x c-j" . consult-dir-jump-file)))

;; ======================================
;;; embark
;; ======================================
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; ======================================
;;; embark-consult
;; ======================================
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; =======================================
;;; electric-pair-mode
;; ======================================-
(use-package electric
  :ensure nil  ;built in
  :config
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\` . ?\`)  ;; 이 라인은 백틱(`)를 쌍으로 추가
                              (?\' . ?\') 
                              (?\{ . ?\})
                              (?\[ . ?\])
                              (?\( . ?\))))
  (electric-pair-mode t))

;; ======================================
;;; rainbow-delimiters
;; ======================================
;; 괄호, 중괄호, 각종 쌍을 시각적(무지개색) 구분
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; =======================================
;;; corfu
;; ======================================-
(setq completion-cycle-threshold 3
      tab-always-indent 'complete)
(use-package corfu
  :ensure t
  :bind (:map corfu-map
	      ("M-d" . corfu-info-documentation)  ; eldoc 정보 표시
              ("M-l" . corfu-info-location)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ("S-<return>" . corfu-insert))
  :custom
  (corfu-auto t)
  ;; (corfu-auto-delay 0.2)
  ;; (corfu-auto-prefix 0)
  (corfu-cycle t)			
  ;; (corfu-preselect 'prompt)
  ;; (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)			        ;orderless field separator
  (corfu-quit-no-match 'separator)	;Automatically quit if there is no match
  :init
  (global-corfu-mode)
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  :hook (eshell-mode . (lambda ()
                         (setq-local corfu-auto t)
                         (corfu-mode 1))))

;; =======================================
;;; eldoc
;; ======================================-
;; 현재 커서 위치의 함수나 변수에 대한 문서를 실시간으로 표시
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . eldoc-mode))

;; =======================================
;;; abbrev
;; ======================================-
;; https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/
(use-package abbrev
  :ensure nil				;built-in
  :config
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  ;; (setq save-abbrevs 'silently)        ;; save abbrevs when files are saved
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")
  (define-abbrev-table 'global-abbrev-table 
    '(("m2"  "㎡")   ("km"  "㎞")  ("lDot" "……") 
      ("cX"    "×")   ("cB"   "※")  ("lDash" "―")
      ("lG"    "「")   ("rG"    "」")
      ("llG"  "『")   ("rrG"   "』")
      ("cZ"   "○")   ("cQ"   "□")
      ))
;; Org 모드 약어 테이블 설정
  (with-eval-after-load 'org
    (define-abbrev-table 'org-mode-abbrev-table
      '(("Dsc" "#+DESCRIPTION: ")
	("Author" "#+AUTHOR: ")
	("Keyword" "#+KEYWORDS: ")
	("Setfile"   "#+SETUPFILE: setLTH/Header.org")
	("SetfileQV"   "#+SETUPFILE: setLTH/Header_quote_verse.org")
	("Option"  "#+OPTIONS: toc:2 num:2")
	("Latex_header"  "#+LATEX_HEADER:")
	)))
  )

;; =======================================
;;; hippie-exp
;; ======================================-
(use-package hippie-exp
  :ensure nil  ; built-in
  :bind ("s-;" . hippie-expand) 
  :hook (org-mode . (lambda ()
                      (make-local-variable 'hippie-expand-try-functions-list)
                      (add-to-list 'hippie-expand-try-functions-list 'try-expand-org-keyword t)))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-whole-kill
          try-expand-list
          try-expand-line
          try-complete-file-name-partially
          try-complete-file-name))
  ;; 특정 모드 무시 설정 (참고용으로만 유지, 실제로는 사용되지 않음)
  (setq hippie-expand-ignore-buffers
        '(archive-mode image-mode doc-view-mode pdf-view-mode tags-table-mode))

  ;; for org-mode 
  (defun try-expand-org-keyword (old)
    "Org-mode 키워드 완성 함수"
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (setq he-expand-list
            (let ((completion-ignore-case t))
              (all-completions he-search-string org-keywords))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (when old (he-reset-string))
               nil)
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

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
(defun my/flush-empty-lines-in-region (start end)
  "Delete all empty or whitespace-only lines in the region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end))

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

(defun my/delete-trailling-whitespace-line-join (&optional start end)
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

;; =======================================
;;; Hunspell 설정
;; ======================================-
;; 한글 맞춤법
(use-package ispell
  :if my-mactop-p
  :hook (text-mode . flyspell-mode)
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "ko_KR")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8))))


;; end here
(provide 'my-completion)

