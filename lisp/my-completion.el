;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-completion.el


;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :demand t
  :config (vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 15))

;; ======================================
;;; marginalia
;; ======================================
(use-package marginalia
  :demand t
  :config (marginalia-mode)
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
	 ("M-y" . consult-yank-pop)
	 ("M-s g" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s m" . consult-imenu)
         ("M-s o" . consult-outline)
	 ("M-g M-g" . consult-goto-line))
  :config
  (setq consult-preview-key '(:debounce 0.5 any)))    ;default 'any

;; =======================================
;;; wgrep
;; =======================================
(use-package wgrep
  :ensure nil
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; =======================================
;;; embark
;; =======================================
(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)         ;; 가장 기본적인 '행동'
         ;; ("M-." . embark-dwim)        ;; 알아서 가장 적절한 '행동' 수행
         ("C-h B" . embark-bindings)) ;; 현재 모드에서 가능한 모든 키 바인딩 확인
  :init
  (setq prefix-help-command #'embark-prefix-help-command)         ;; 미니버퍼 내에서 도움말 가능하도록
  :config
  (advice-add 'embark-act :before #'my/deactivate-input-method))  ;; 기본(en) input-method 전환

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; =======================================
;;; pair-pair-wrap
;; =======================================
;;inspire https://protesilaos.com
(defcustom my-pair-pairs
  '((?* :description "Bold"           :pair ?*)
    (?/ :description "Italic"         :pair ?/)
    (?= :description "Verbatim"       :pair ?=)
    (?~ :description "Code"           :pair ?~)
    (?+ :description "Strike"         :pair ?+)
    (?_ :description "Under Line"     :pair ?_)
    (?\" :description "Double Quotes" :pair ?\")
    (?\' :description "Single Quotes" :pair ?\')
    (?\( :description " () "          :pair (?\( . ?\)))
    (?\[ :description " [] "          :pair (?\[ . ?\]))
    (?{  :description " {} "          :pair (?{ . ?}))
    (?<  :description "「」"          :pair ("「" . "」"))
    (?>  :description "『』"          :pair ("『" . "』"))
    (?M  :description "《》"          :pair ("《" . "》")))
  "List of Org-mode emphasis markers and special bracket pairs."
  :group 'editing
  :type '(alist :key-type character :value-type (plist)))

;; (defun my--enable-tab-escape ()
;;   "Enable a temporary TAB binding to jump out of brackets or emphasis markers"
;;   (set-transient-map
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd "TAB")
;;                  (lambda () (interactive) (forward-char 1)))
;;      map)
;;    t))

(defun my-pair-pairs-wrap (char &optional _target)
  "Enclose the active region or the word at point with a pair of CHARs."
  (interactive "c기호 입력 (*, /, =, (, <...): ")
  (let* ((entry (assoc char my-pair-pairs))
         (pair-data (plist-get (cdr entry) :pair))
         (open (if (consp pair-data) (car pair-data) pair-data))
         (close (if (consp pair-data) (cdr pair-data) pair-data))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
         (start (car bounds))
         (end (cdr bounds)))

    (if (not pair-data)
        (message "Undefined symbol: %c" char)
      (save-excursion
        (goto-char end)              ;뒤쪽 닫는 기호 삽입
        (insert (if (characterp close) (char-to-string close) close))
        (goto-char start)            ;앞쪽 여는 기호 삽입
        (insert (if (characterp open) (char-to-string open) open)))

      ;; (my--enable-tab-escape)
      (message "'%s' 완료" (plist-get (cdr entry) :description)))))

(with-eval-after-load 'embark
  (dolist (map (list embark-symbol-map
                     embark-region-map
                     embark-general-map))
    (define-key map (kbd "w") #'my-pair-pairs-wrap)))

;; =======================================
;;; Hunspell 설정
;; =======================================
;; (defun my-korean-spell-check ()
;;   "Set hunspell as the default spell checker for Korean"
;;   (interactive)
;;   (require 'ispell) ;; 함수 실행 시 패키지 로드
;;   (setq ispell-local-dictionary "ko_KR")
;;   (flyspell-mode 1)
;;   (message "Korean spell check enable"))

;; (use-package ispell
;;   :if my-macOS-p
;;   :defer t
;;   :config
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-local-dictionary-alist
;;         '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8)))
;;   (setq flyspell-delay 0.5)
;;   (setq flyspell-issue-message-flag nil)
;;   (setq flyspell-use-meta-tab nil))

;; =======================================
;;; completion-preview
;; =======================================
(use-package completion-preview
  :ensure nil
  :defer t 
  :config
  (global-completion-preview-mode)
  (push 'org-self-insert-command completion-preview-commands))

;; =======================================
;;; abbrev
;; =======================================
(use-package abbrev
  :ensure nil
  :hook (org-mode . abbrev-mode)
  :custom (save-abbrevs nil)
  :config
  (with-eval-after-load 'org
    ;; (abbrev-table-put org-mode-abbrev-table 
    ;;                   :regexp "\\(?:^\\|[\t\s]+\\)\\(?1::.*\\)")
    (define-abbrev-table 'org-mode-abbrev-table
      '(;; 특수문자
        ("rA" "→")("lA" "←") ("rrA" "⇒")("llA" "⇐")("lS" "―") ("lT" "……")
        ("lG" "「") ("rG" "」") ("cD" "·") ("llG" "『") ("rrG" "』")
        ;; Org-mode 설정
        ("Dsc"     "#+DESCRIPTION: ")
        ("Title"   "#+TITLE: ")
        ("Author"  "#+AUTHOR: ")
        ("Keyword" "#+KEYWORDS: ")
        ("Setfile" "#+SETUPFILE: setLTH/Header.org")
        ("Center"  "#+BEGIN_CENTER\n· · ·\n#+END_CENTER")
        ("Nonum"   ":PROPERTIES:\n:UNNUMBERED: t\n:END:")
        ("Option"  "#+OPTIONS: toc:2 num:2 d:nil")
	("Qsty"    "#+QUOTE_STYLE:")
        ("Grayq"   "#+ATTR_LATEX: :environment grayquote")
        ("Doimg"   "#+ATTR_LATEX: :width 0.5\\textwidth\n#+CAPTION: \n")
	("Right"   "#+BEGIN_EXPORT latex\n\\begin{flushright}\n\n\\end{flushright}\n#+END_EXPORT")
	("Cover" "#+begin_src emacs-lisp :exports results :results none :eval export
                  (make-variable-buffer-local 'org-latex-title-command)
                  (setq org-latex-title-command (concat
                        \"\\\\begin{titlepage}\\n\"
                        \"\\\\includegraphics[width=14.7cm]{./img/PATH}\\n\"
                        \"\\\\end{titlepage}\\n\"))
                  #+end_src")
	("Bskip" "#+LATEX: \\bigskip")
	("Mskip" "#+LATEX: \\medskip")
	("Nskip" "#+LATEX: \\vspace{\\baselineskip}")
        ("Notoc" "#+LATEX: \\addcontentsline{toc}{section}{}" 
         (lambda () (backward-char 1)))))))




;; end here
(provide 'my-completion)
