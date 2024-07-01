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
    '( ("ldot"        " ……")
       ("notosans"     "Noto Sans CJK KR")
       ("notosansmono"   "Noto Sans Mono CJK KR" )
       ("notoserif"      "Noto Serif CJK KR")
       ("hcrbatang"    "HCRBatangLVT")
       ("sectionroman"   "\renewcommand*{\thesection}{\Roman{section}")
       ("subsectionarabic"  "\renewcommand*{\thesubsection}{\arabic{subsection}")
       ("b_q" "#+bebin_quote")
       ("e_q" "#+end_quote")
       )))

(defun select-special-character ()
  "Prompt the user to select a special character and insert it at point."
  (interactive)
  (let ((characters '("·" "→" "「」" "※")))
    (insert (completing-read "Select special character: " characters))))




;; end here
(provide 'my-completion)

