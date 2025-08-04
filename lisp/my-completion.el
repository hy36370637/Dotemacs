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
;;	 ("C-c SPC d" . consult-dir)
	 ("C-c SPC f" . consult-find)
	 ("C-c SPC g" . consult-grep)
	 ("C-c SPC l" . consult-line))
;;	 ("C-c SPC r" . consult-register))
  :config
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file))
  (setq consult-preview-key 'any)
  ;; 북마크 정렬 설정
  (setq consult-sort-function #'consult--alpha-sort)
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; ======================================
;;; consult-dir
;; ======================================
;; (use-package consult-dir
;;   :ensure nil)
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
  :defer nil
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
  ;;(abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[ \t]+\\)[:;_]?\\(\\w+\\)")
  ;;줄 시작이나 공백/탭 다음에 오는, 선택적으로 :, ;, _로 시작하고,  뒤에 단어가 이어지는 형태의 약어만 확장
  (define-abbrev-table 'global-abbrev-table 
    '(("m2"  "㎡")   ("km"  "㎞")  ("lDot" "……") 
      ("cA"    "→")   ("cB"   "※")  ("lDash" "―")
      ("lG"    "「")   ("rG"    "」")  ("pC" "·")
      ("llG"  "『")   ("rrG"   "』")  
      ("cZ"   "○")   ("cQ"   "□")
      ))
;;; Org 모드 약어 테이블 설정
  (with-eval-after-load 'org
    (define-abbrev-table 'org-mode-abbrev-table
      '(("Dsc" "#+DESCRIPTION: ")
	("Title"   "#+TITLE:")
	("Author" "#+AUTHOR: ")
	("Keyword" "#+KEYWORDS: ")
	("Setfile"   "#+SETUPFILE: setLTH/Header.org")
	("SetfileQV"   "#+SETUPFILE: setLTH/HeaderQV.org")
	("sDot"   "#+begin_center\n· · ·\n#+end_center")
	("Option"  "#+OPTIONS: toc:2 num:2")
	("Latex_header"  "#+LATEX_HEADER:")
	("Inimg" "#+attr_latex: :width 0.5\\textwidth\n#+CAPTION: \n[[../org/img/imgJW/IMG_]]")
	))))

;; =======================================
;;; hippie-exp
;; ======================================-
;; **hippie-expand 패키지 설정 (dabbrev 관련 설정 통합)**
(use-package hippie-expand
  :ensure nil ; 내장 패키지이므로 ensure nil
  :bind (("s-;" . hippie-expand)) ;
  :config
  ;; hippie-expand가 시도할 확장 함수들의 순서를 정의
  (setq hippie-expand-try-functions-list
        '(try-expand-all-abbrevs         ; 1. 정의된 약어 (abbrev 설정)
          try-expand-dabbrev             ; 2. 현재 및 열린 다른 버퍼의 단어
          try-expand-dabbrev-all-buffers ; 3. 모든 버퍼의 단어
          try-expand-dabbrev-from-kill   ; 4. 킬 링 (복사/잘라내기 이력) 내용
          try-complete-file-name-partially ; 5. 파일 이름 부분 완성
          try-complete-file-name         ; 6. 파일 이름 전체 완성
          try-expand-list                ; 7. 괄호 안의 인자 목록 등 확장
          try-expand-line                ; 8. 현재 줄과 유사한 과거 줄 확장
          try-complete-lisp-symbol-partially ; 9. Emacs Lisp 심볼 부분 완성 (Elisp 코딩 시)
          try-complete-lisp-symbol       ; 10. Emacs Lisp 심볼 전체 완성
          try-complete-lisp-variable))   ; 11. Emacs Lisp 변수 완성
  ;; dabbrev 동작 방식 미세 조정 (hippie-expand의 dabbrev 함수들에 영향을 줍니다)
  ;; (setq dabbrev-case-replace t)    ; 확장할 때 원본 단어의 대소문자를 유지
  (setq dabbrev-minimum-length 3)  ; 최소 3글자 이상 입력해야 확장을 시도
  )



;; end here
(provide 'my-completion)

