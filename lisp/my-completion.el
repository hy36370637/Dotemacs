;; 완성completion 도움
;; .emacs.d/lisp/my-completion.el

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
;; (add-hook 'org-mode-hook (lambda ()
;;                            (setq-local electric-pair-inhibit-predicate
;;                                        (lambda (c)
;;                                          (if (char-equal c ?<)
;;                                              t
;;                                            (funcall (default-value 'electric-pair-inhibit-predicate) c))))))

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
  :ensure nil
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
	("Setupfile" "#+SETUPFILE: setLTH/Header.org")
	("Option"  "#+OPTIONS: toc:2 num:2")
	)))
  )

;; =======================================
;;; 특수문자 입력
;; ======================================-
(defun select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((choi '("·"  "→"  "⇒"  "「」"  "『』"  "※"  "…"  "―")))
    (insert (completing-read "선택: " choi))))

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

