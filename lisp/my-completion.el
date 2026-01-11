;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-completion.el
;; ======================================
;;; vertico
;; ======================================
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 15))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-width 100)
  (vertico-posframe-border-width 2)
  (vertico-posframe-parameters
   '((left-fringe . 20)
     (right-fringe . 20)))
  :init
  (defun my-vertico-posframe-update-border-color ()
    "Apply current theme's highlight color to vertico-posframe border."
    (when (facep 'vertico-posframe-border)
      (set-face-attribute 'vertico-posframe-border nil 
                          :background (face-attribute 'highlight :background nil t))))
  :config
  ;; Ensure center position
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-mode 1)
  ;; Apply initially and register hook
  (my-vertico-posframe-update-border-color)
  (add-hook 'after-load-theme-hook #'my-vertico-posframe-update-border-color))

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
	 ("C-c b" . consult-bookmark)
	 ("M-y" . consult-yank-pop)
	 ("M-s g" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s m" . consult-imenu)
         ("M-s o" . consult-outline)
	 ("M-g M-g" . consult-goto-line))
  :config
  (setq consult-buffer-sources (list consult--source-buffer
				     consult--source-recent-file)))

;; =======================================
;;; embark
;; ======================================-
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; 가장 기본적인 '행동'
         ("M-." . embark-dwim)        ;; 알아서 가장 적절한 '행동' 수행
         ("C-h B" . embark-bindings)) ;; 현재 모드에서 가능한 모든 키 바인딩 확인
  :init
  ;; 미니버퍼 내에서 도움말 역할을 하도록 설정
(setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; =======================================
;;; electric-pair-mode
;; ======================================-
(use-package electric
  :ensure nil
  :init
  (electric-pair-mode t)
  :custom
  ;; 모든 모드에서 공통으로 사용할 기본 괄호 설정
  (electric-pair-pairs '((?\" . ?\")
                         (?\' . ?\')
                         (?\{ . ?\})
                         (?\[ . ?\])
                         (?\( . ?\))))
  :hook
  ;; Org-mode가 실행될 때만 실행될 로직
  (org-mode . (lambda ()
                (setq-local electric-pair-pairs 
                            (append electric-pair-pairs
                                    '((?* . ?*)   
                                      (?/ . ?/)   
                                      (?= . ?=)   
                                      (?~ . ?~)   
                                      (?+ . ?+)))))))

;; =======================================
;;; 특수문자 입력
;; ======================================-
;;inspire https://protesilaos.com
(defcustom my-pair-pairs
  '((?* :description "Bold"           :pair ?*)
    (?/ :description "Italic"         :pair ?/)
    (?= :description "Verbatim"       :pair ?=)
    (?~ :description "Code"           :pair ?~)
    (?+ :description "Strike"         :pair ?+)
    (?\" :description "Double Quotes" :pair ?\")
    (?\' :description "Single Quotes" :pair ?\')
    (?\( :description " () "          :pair (?\( . ?\)))
    (?\[ :description " [] "          :pair (?\[ . ?\]))
    (?{  :description " {} "          :pair (?{ . ?}))
    (?<  :description "「」"          :pair ("「" . "」"))
    (?>  :description "『』"          :pair ("『" . "』")))
;;    (?M  :description "《》"          :pair ("《" . "》")))
  "List of Org-mode emphasis markers and special bracket pairs."
  :group 'editing
  :type '(alist :key-type character :value-type (plist)))

(defun my--enable-tab-escape ()
  "Enable a temporary TAB binding to jump out of brackets or emphasis markers"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "TAB")
                 (lambda () (interactive) (forward-char 1)))
     map)
   t))

(defun my-pair-pairs-wrap (char)
  "Enclose the active region or the word at point with a pair of CHARs."
  (interactive "c기호 입력 (*, /, =, ~, (, [, <, > ...): ")
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
        ;; 닫는 기호 삽입
        (goto-char end)
        (insert (if (characterp close) (char-to-string close) close))
        ;; 여는 기호 삽입
        (goto-char start)
        (insert (if (characterp open) (char-to-string open) open)))
      
      ;; 커서 위치 조정 및 TAB 탈출 기능 활성화
      (unless (use-region-p) (goto-char (1+ end)))
      (my--enable-tab-escape)
      (message "'%s' 완료 (TAB으로 탈출)" (plist-get (cdr entry) :description)))))

;; =======================================
;;; Hunspell 설정
;; ======================================-
(defun my-korean-spell-check ()
  "Set hunspell as the default spell checker for Korean"
  (interactive)
  (require 'ispell) ;; 함수 실행 시 패키지 로드
  (setq ispell-local-dictionary "ko_KR")
  (flyspell-mode 1)
  (message "Korean spell check enable"))

(use-package ispell
  :if my-macOS-p
  :defer t
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        '(("ko_KR" "[가-힣]" "[^가-힣]" "[-']" nil ("-d" "ko_KR") nil utf-8)))
  (setq flyspell-delay 0.5)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-use-meta-tab nil))

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
    ;; (abbrev-table-put org-mode-abbrev-table 
    ;;                   :regexp "\\(?:^\\|[\t\s]+\\)\\(?1::.*\\)")
    (define-abbrev-table 'org-mode-abbrev-table
      '(;; 특수문자
        ("rA" "→") ("lS" "―") ("lT" "……")
        ("lG" "「") ("rG" "」") ("cD" "·") ("llG" "『") ("rrG" "』")
        ;; Org-mode 설정
        ("Dsc" "#+DESCRIPTION: ")
        ("Title" "#+TITLE: ")
        ("Author" "#+AUTHOR: ")
        ("Keyword" "#+KEYWORDS: ")
        ("Setfile" "#+SETUPFILE: setLTH/Header.org")
        ("Center" "#+BEGIN_CENTER\n· · ·\n#+END_CENTER")
        ("Nonum" ":PROPERTIES:\n:UNNUMBERED: t\n:END:")
        ("Option" "#+OPTIONS: toc:2 num:2")
        ("Grayq" "#+ATTR_LATEX: :environment grayquote")
        ("Doimg" "#+ATTR_LATEX: :width 0.5\\textwidth\n#+CAPTION: \n")
	("Right" "#+BEGIN_EXPORT latex\n\\begin{flushright}\n\n\\end{flushright}\n#+END_EXPORT")
	("Cover" "#+begin_src emacs-lisp :exports results :results none :eval export
                  (make-variable-buffer-local 'org-latex-title-command)
                  (setq org-latex-title-command (concat
                        \"\\\\begin{titlepage}\\n\"
                        \"\\\\includegraphics[width=14.7cm]{./img/PATH}\\n\"
                        \"\\\\end{titlepage}\\n\"))
                  #+end_src")
        ("Notoc" "#+LATEX: \\addcontentsline{toc}{section}{}" 
         (lambda () (backward-char 1)))))))  ;; Notoc: 입력 후 커서를 {} 사이로 이동






;; end here
(provide 'my-completion)
