;;; my-search.el --- Web search and local file search unified -*- lexical-binding: t; -*-

(require 'url)
(require 'dom)
(require 'cl-lib)
(require 'consult)

;; ======================================
;;; Configuration
;; ======================================

(defvar my-search-engines
  '(("macOS Dictionary" . dict)
    ("Naver" .  "https://search.naver.com/search.naver?query=%s")
    ("Naver Hanja" . "https://hanja.dict.naver.com/#/search?range=all&query=%s")
    ;; ("Namuwiki" .  "https://namu.wiki/w/%s")
    ("Google" . "https://www.google.com/search?q=%s"))
  "List of web search engines (Name . URL/Type).")

(defvar my-search-path-targets
  '(("Docs (All)"    . "~/Dropbox/Docs/")
    ("Org Files"     . "~/Dropbox/Docs/org/")
    ("PDF Files"     . "~/Dropbox/Docs/pdf/") 
    ("Notes/Person"  . "~/Dropbox/Docs/Person/")
    ("Denote"        . "~/Dropbox/Docs/org/denote/")
    ("Emacs Config"  . "~/.emacs.d/"))
  "List of local directories for ripgrep search.")

;; ======================================
;;; Helper Functions
;; ======================================

(defun my--get-search-url (engine query)
  "Generate a search URL from a search engine and a query."
  (let ((config (assoc engine my-search-engines)))
    (when config
      (let ((url-template (cdr config)))
        (if (eq url-template 'dict)
            (concat "dict://" (url-hexify-string query))
          (format url-template (url-hexify-string query)))))))

(defun my--open-url (url)
  "Open URL in the default web browser or macOS Dictionary."
  (if (string-prefix-p "dict://" url)
      (call-process "open" nil 0 nil url)
    (browse-url url)))

(defun my-rga-skim-search (&optional query)
  "Search PDF contents using `rga` and open in Skim."
  (interactive)
  (let* ((pdf-target (assoc "PDF Files" my-search-path-targets))
         (default-directory (expand-file-name (cdr pdf-target)))
         ;; query 인자가 있으면 사용, 없으면 직접 입력 받음
         (search-term (or query (read-string "Search PDFs: ")))
         (results (split-string
                   (shell-command-to-string
                    (format "rga -l %s ." (shell-quote-argument search-term)))
                   "\n" t)))
    (if (null results)
        (message "결과 없음: '%s'" search-term)
      (catch 'exit
        (while t
          (let ((selected-file
                 (condition-case nil
                     (completing-read (format "파일 선택 [%s]: " search-term) results nil t)
                   (quit (throw 'exit nil)))))
            (condition-case nil
                (let* ((full-path (expand-file-name selected-file default-directory))
                       (lines (split-string
                               (shell-command-to-string
                                (format "rga -n %s %s"
                                        (shell-quote-argument search-term)
                                        (shell-quote-argument full-path)))
                               "\n" t))
                       (selected-line (completing-read "결과 내 이동 (C-g=뒤로): " lines nil t)))
                  (kill-new search-term)
                  (let ((page-num (when (string-match "Page \\([0-9]+\\)" selected-line)
                                   (string-to-number (match-string 1 selected-line)))))
                    (if page-num
                        (do-applescript
                         (format "tell application \"Skim\"
    activate
    open POSIX file \"%s\"
    tell front document to go to page %d
end tell" full-path page-num))
                      (call-process "open" nil 0 nil full-path)))
                  (throw 'exit nil))
              (quit (message "파일 목록으로 복귀")))))))))

;; ======================================
;;; Main Function
;; ======================================
;;; ###autoload
(defun my-search-unified (&optional query)
  "Unified search interface for Web and Local files.
Select between Web engines or Local paths for the given QUERY."
  (interactive 
   (list (read-string "Search query: " (thing-at-point 'symbol t))))
  (let* (;; 전달받은 query가 없으면(직접 실행 시) interactive에서 받은 값 사용
         (search-term (or query (thing-at-point 'symbol t)))
         (web-options (mapcar #'car my-search-engines))
         (local-options (mapcar #'car my-search-path-targets))
         (all-options (append web-options local-options))
         (choice (completing-read (format "Search '%s' in: " search-term) all-options))
         (web-config (assoc choice my-search-engines))
         (local-path (cdr (assoc choice my-search-path-targets))))
    (cond
     ;; CASE 1: 웹 엔진 선택
     (web-config
      (let ((url (my--get-search-url choice search-term)))
        (if (and url (not (string-empty-p url)))
            (my--open-url url)
          (message "잘못된 URL 설정."))))
     
     ;; CASE 2: PDF 로컬 경로 선택
     ((and local-path (string-match-p "PDF" choice))
      (my-rga-skim-search search-term))
     
     ;; CASE 3: 일반 로컬 경로 선택
     (local-path
      (let ((default-directory (expand-file-name local-path)))
        (consult-ripgrep default-directory search-term)))
     
     (t (message "알 수 없는 선택.")))))

;; ======================================
;;; Embark Integration
;; ======================================

(with-eval-after-load 'embark
  (let ((target-maps (list embark-identifier-map embark-region-map)))
    (dolist (map target-maps)
      (define-key map (kbd "S") #'my-search-unified))))

(provide 'my-search)
;;; my-search.el ends here
