;; 완성completion 도움
;; emacs/lisp/my-completion.el

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
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ("S-<return>" . corfu-insert))
  :custom
  (corfu-auto t)                   ;enable auto completion
  ;; (corfu-auto-delay 0.2)
  ;; (corfu-auto-prefix 0)
  (corfu-cycle t)			
  ;; (corfu-preselect 'prompt)
  ;; (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)			;orderless field separator
  (corfu-quit-no-match 'separator)	;Automatically quit if there is no match
  :init
  (global-corfu-mode)
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  :hook (eshell-mode . (lambda ()
                         (setq-local corfu-auto t)
                         (corfu-mode 1))))

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
    '(("m2"  "㎡")   ("km"  "㎞")("lDot" "……") 
      ("cX"    "×")   ("cB"   "※")
      ("lG"    "「")   ("rG"    "」")
      ("llG"  "『")   ("rrG"   "』")
      ("cZ"   "○")   ("cQ"   "□")
      ))
;; Org 모드 약어 테이블 설정
  ;; (with-eval-after-load 'org
  ;;   (define-abbrev-table 'org-mode-abbrev-table
  ;;     '(("dsc" "#+DESCRIPTION:")
  ;; 	("doimg" "#+ATTR_LATEX: :width 0.4\\textwidth\n#+CAPTION: \n   [[.img/imgDir/.jpg]]")
  ;; 	("setupfile" "#+SETUPFILE: setLTH/Header.org")
  ;; 	("mktitle" "#+begin_src emacs-lisp :exports results :results none :eval export\n(make-variable-buffer-local 'org-latex-title-command)\n (setq org-latex-title-command(concat\n\"\\\\begin{titlepage}\\n\"\n\"\\\\includegraphics[width=14.7cm]{./img/imgCover/}\\n\"\n\"\\\\end{titlepage}\\n\"))\n#+end_src\n")
  ;;     )))
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

