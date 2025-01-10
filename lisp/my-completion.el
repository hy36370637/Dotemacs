;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-completion.el
;;
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
  :bind (("C-x b" . consult-buffer)
	 ("C-x C-r" . consult-recent-file)
	 ("C-c SPC b" . consult-bookmark)
	 ("C-c SPC d" . consult-dir)
	 ("C-c SPC f" . consult-find)
	 ("C-c SPC g" . my-consult-grep-custom)
	 ("C-c SPC l" . consult-line)
	 ("C-c SPC r" . consult-register))
  :init
  (defun my-consult-grep-custom ()
    "Run consult-grep with option to use default directory or choose a new one."
    (interactive)
    (let* ((default-dir (or org-directory (expand-file-name "~/Dropbox/Docs/org/")))
           (use-default (y-or-n-p (format "Use default directory (%s)? " default-dir)))
           (dir (if use-default
                    default-dir
                  (read-directory-name "Choose directory: " nil nil t))))
      (consult-grep dir)))
  :config
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file))
  ;; ;; 북마크 관련 추가 설정
  ;; (setq consult-bookmark-narrow
  ;;       '((?b "Bookmarks" bookmark)
  ;;         (?f "Files" file)
  ;;         (?d "Directories" dir)))
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
                              (?\` . ?\`)
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
;;; completion-preview
;; ======================================-
(use-package completion-preview
  :ensure nil				;version 30
  :config
  (global-completion-preview-mode)
  ;; enable completion-preview in org-mode
  (push 'org-self-insert-command completion-preview-commands))

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
      ("cA"    "→")   ("cB"   "※")  ("lDash" "―")
      ("lG"    "「")   ("rG"    "」") 
      ("llG"  "『")   ("rrG"   "』")
      ("cZ"   "○")   ("cQ"   "□")
      ))
;; Org 모드 약어 테이블 설정
  (with-eval-after-load 'org
    (define-abbrev-table 'org-mode-abbrev-table
      '(("Dsc" "#+DESCRIPTION: ")
	("Title"   "#+TITLE:")
	("Author" "#+AUTHOR: ")
	("Keyword" "#+KEYWORDS: ")
	("Setfile"   "#+SETUPFILE: setLTH/Header.org")
	("SetfileQV"   "#+SETUPFILE: setLTH/HeaderQV.org")
	("Option"  "#+OPTIONS: toc:2 num:2")
	("Latex_header"  "#+LATEX_HEADER:")
	("Inimg" "#+attr_latex: :width 0.4\\textwidth\n#+CAPTION: \n[[../org/img/imgDaily/fileName]]")
	))))

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

  (defun try-expand-org-keyword (old)
    "Org-mode 키워드 자동완성 함수"
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


;; end here
(provide 'my-completion)

