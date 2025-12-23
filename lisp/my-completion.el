;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-completion.el
;;
;; ======================================
;;; which-key
;; ======================================
(use-package which-key
  :ensure nil
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.2))

;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize nil)    ; 크기 조정 비활성화로 성능 
  (vertico-cycle t)
  (vertico-count 20))   ; 표시할 항목 수 제한
 
;; ======================================
;;; marginalia
;; ======================================
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init (marginalia-mode)
  :custom
  (marginalia-align 'right)
  (marginalia-align-offset 0))

;; ======================================
;;; orderless
;; ======================================
(use-package orderless
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
	 ("C-c B" . consult-bookmark)
	 ("C-c R" . consult-register)
	 ("M-s f" . consult-find)
	 ("M-s g" . consult-grep)
	 ("M-s l" . consult-line))
  :config
  (setq consult-buffer-sources (list consult--source-buffer
				     consult--source-recent-file))
  (setq consult-preview-key 'any))

;; ======================================
;;; consult-dir
;; ======================================
(use-package consult-dir
  :ensure nil
  :after (vertico)
  :init
  (setq enable-recursive-minibuffers t) ;; 재귀적 미니버퍼
  :bind (("C-c D" . consult-dir)
          :map vertico-map
         ("C-c D" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

;; =======================================
;;; electric-pair-mode
;; ======================================-
(use-package electric
  :ensure nil
  :init (electric-pair-mode t)
  :custom
  (electric-pair-pairs '((?\" . ?\")
                         (?\` . ?\`)
                         (?\' . ?\')
                         (?\{ . ?\})
                         (?\[ . ?\])
                         (?\( . ?\)))))

;; =======================================
;;; completion-preview
;; ======================================-
(use-package completion-preview
  :ensure nil
  :init (global-completion-preview-mode)
  :config
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
(use-package abbrev
  :ensure nil
  :hook (org-mode . abbrev-mode)
  :custom (save-abbrevs nil)
  :config
  (with-eval-after-load 'org
    (define-abbrev-table 'org-mode-abbrev-table
      '(;; 특수문자
        ("rA" "→") ("lS" "―") ("lT" "……")
        ("lG" "「") ("rG" "」") ("cD" "·") ("llG" "『") ("rrG" "』")
        ;; Org-mode 설정
        ("Dsc" "#+DESCRIPTION: ")
        ("Title" "#+TITLE:")
        ("Author" "#+AUTHOR: ")
        ("Keyword" "#+KEYWORDS: ")
        ("Setfile" "#+SETUPFILE: setLTH/Header.org")
        ("sDot" "#+begin_center\n· · ·\n#+end_center")
        ("Unum" ":PROPERTIES:\n:UNNUMBERED: t\n:END:")
        ("Notoc" "#+LATEX: \\addcontentsline{toc}{section}{Dsctitle}")
        ("Option" "#+OPTIONS: toc:2 num:2")
        ("Grayq" "#+ATTR_LATEX: :environment grayquote")
        ("Doimg" "#+attr_latex: :width 0.5\\textwidth\n#+CAPTION: \n[[../org/img/imgJW/IMG_]]")))))

;; end here



(provide 'my-completion)
