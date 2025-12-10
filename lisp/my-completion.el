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
(setq enable-recursive-minibuffers t) ; for consut-dir
(minibuffer-depth-indicate-mode 1)  ;; 선택: 깊이 표시

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
        ("rA" "→") ("cB" "※") ("lDash" "―") ("lDot" "……")
        ("lG" "「") ("rG" "」") ("cD" "·") ("llG" "『") ("rrG" "』")
        ;; Org-mode 설정
        ("Dsc" "#+DESCRIPTION: ")
        ("Title" "#+TITLE:")
        ("Author" "#+AUTHOR: ")
        ("Keyword" "#+KEYWORDS: ")
        ("Setfile" "#+SETUPFILE: setLTH/Header.org")
        ("SetfileQV" "#+SETUPFILE: setLTH/HeaderQV.org")
        ("sDot" "#+begin_center\n· · ·\n#+end_center")
        ("Unum" ":PROPERTIES:\n:UNNUMBERED: t\n:END:")
        ("Notoc" "#+LATEX: \\addcontentsline{toc}{section}{Dsctitle}")
        ("Option" "#+OPTIONS: toc:2 num:2")
        ("Grayq" "#+ATTR_LATEX: :environment grayquote")
        ("Doimg" "#+attr_latex: :width 0.5\\textwidth\n#+CAPTION: \n[[../org/img/imgJW/IMG_]]")))))

;; =======================================
;;; hippie-exp
;; ======================================-
;; **hippie-expand 패키지 설정 (dabbrev 관련 설정 통합)**
;; (use-package hippie-expand
;;   :ensure nil ; 내장 패키지이므로 ensure nil
;;   :bind (("C-c SPC" . hippie-expand)) ;
;;   :config
;;   ;; hippie-expand가 시도할 확장 함수들의 순서를 정의
;;   (setq hippie-expand-try-functions-list
;;         '(try-expand-all-abbrevs         ; 1. 정의된 약어 (abbrev 설정)
;;           try-expand-dabbrev             ; 2. 현재 및 열린 다른 버퍼의 단어
;;           try-expand-dabbrev-all-buffers ; 3. 모든 버퍼의 단어
;;           try-expand-dabbrev-from-kill   ; 4. 킬 링 (복사/잘라내기 이력) 내용
;;           try-complete-file-name-partially ; 5. 파일 이름 부분 완성
;;           try-complete-file-name         ; 6. 파일 이름 전체 완성
;;           try-expand-list                ; 7. 괄호 안의 인자 목록 등 확장
;;           try-expand-line                ; 8. 현재 줄과 유사한 과거 줄 확장
;;           try-complete-lisp-symbol-partially ; 9. Emacs Lisp 심볼 부분 완성 (Elisp 코딩 시)
;;           try-complete-lisp-symbol       ; 10. Emacs Lisp 심볼 전체 완성
;;           try-complete-lisp-variable)   ; 11. Emacs Lisp 변수 완성
;;   ;; dabbrev 동작 방식 미세 조정 (hippie-expand의 dabbrev 함수들에 영향을 줍니다)
;; 	(setq dabbrev-case-replace t)    ; 확장할 때 원본 단어의 대소문자를 유지
;; 	(setq dabbrev-minimum-length 2))  ; 최소 3글자 이상 입력해야 확장을 시도
;;   )

;;
;; (use-package company
;;   :ensure t
;;   :hook (prog-mode . company-mode))

;; (use-package flycheck
;;   :ensure t
;;   :hook (prog-mode . flycheck-mode))


;; end here
(provide 'my-completion)
