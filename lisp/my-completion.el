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

(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       (lambda (c)
                                         (if (char-equal c ?<)
                                             t
                                           (funcall (default-value 'electric-pair-inhibit-predicate) c))))))

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
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-echo-documentation 0.2)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; =======================================
;;; abbrev
;; ======================================-
;; https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/
(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode nil)
  (setq save-abbrevs 'silently)        ;; save abbrevs when files are saved
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")
  (define-abbrev-table 'global-abbrev-table 
    '(("notosans"     "Noto Sans CJK KR")
       ("notosansmono"   "Noto Sans Mono CJK KR" )
       ("notoserif"      "Noto Serif CJK KR")
       ("hcrbatang"    "HCRBatangLVT")
       ("sectionroman"   "\renewcommand*{\thesection}{\Roman{section}")
       ("subsectionarabic"  "\renewcommand*{\thesubsection}{\arabic{subsection}")
       ("b_q" "#+bebin_quote")
       ("e_q" "#+end_quote")
       )))

;; =======================================
;;; 특수문자 입력
;; ======================================-
(defun select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((ch '("·" "→" "「」" "※" "…" "―")))
    (insert (completing-read "문자 선택: " ch))))

;; =======================================
;;; Hunspell 설정
;; ======================================-
;; 한글 맞춤법
(when (equal (system-name) "MacBookAir.local")
  ;; Hunspell 설정
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "ko_KR")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8))))

;; 맞춤법 검사 함수 정의 - 수동 검사
(defun my-hunspell-check ()
  "Run Hunspell spell check on the current buffer."
  (interactive)
  (ispell-buffer))

;; Flyspell 모드 활성화 방법
;;(add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)



;; end here
(provide 'my-completion)

