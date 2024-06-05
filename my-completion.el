;; 완성completion 도움
;; emacs/lisp/my-completion.el

;; =======================================
;;; electric-pair-mode
;; ======================================-
(use-package electric
  :ensure nil  ;built in
  :config
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\[ . ?\])
                              (?\( . ?\))))
  (electric-pair-mode t))
(add-hook 'org-mode-hook (lambda ()   ; pair-mode '< '제외
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

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
  (setq-default abbrev-mode t)
  (setq save-abbrevs 'silently)        ;; save abbrevs when files are saved
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")
  ;; 약어 정의
  (define-abbrev-table 'global-abbrev-table 
    '( ("ldot"        " ……")
       ("notosans"     "Noto Sans CJK KR")
       ("notosansmono"  "Noto Sans Mono CJK KR" )
       ("notoserif"    "Noto Serif CJK KR")
       ("hcrbatang"  "HCRBatangLVT")
       ("sectionroman"   "\renewcommand*{\thesection}{\Roman{section}")
       ("subsectionarabic"  "\renewcommand*{\thesubsection}{\arabic{subsection}")
       )))
  


(provide 'my-completion)

