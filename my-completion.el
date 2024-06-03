;; 완성completion 도움

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
(use-package abbrev
  :ensure nil
  :hook ((text-mode) . abbrev-mode)
  :config
  (setq only-global-abbrevs nil)
  ;; 약어 정의
(define-abbrev-table 'text-mode-abbrev-table '(    ;global-abbrev-table
    ;; 일반적인 약어
    ("arrR"      "→")
    ("lrgg"       "「」")
    ("pitC"       "·"))))
  


(provide 'my-completion)
