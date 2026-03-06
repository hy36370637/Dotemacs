;;; my-dired-custom.el --- Custom Dired configuration -*- lexical-binding: t; -*-

;; ======================================
;;; Helfer Function
;; ======================================
(defcustom my-dired-external-regexp "\\.pdf\\|\\.docx\\|\\.xlsx\\|\\.hwp\\|\\.hwpx\\|\\.mp4\\|\\.png\\|\\.jpg"
  "외부 프로그램으로 연결할 확장자 목록."
  :type 'string
  :group 'dired)

(defun my-dired-open-dwim ()
  "파일 확장자에 따라 Emacs 내부 혹은 macOS 외부 앱(open)으로 실행합니다."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (ext (file-name-extension file t)))
    (if (and ext (string-match-p my-dired-external-regexp ext))
        ;; 외부 앱으로 실행 (macOS 'open' 명령어)
        (progn
          (start-process "dired-external-open" nil "open" file)
          (message "외부 앱 실행: %s" (file-name-nondirectory file)))
      ;; 그 외(텍스트, 디렉토리 등)는 Emacs 내부에서 실행
      (if (file-directory-p file)
          (dired-find-file)
        (find-file file)))))


(defun my-dired-move-to-pdf-folder-safe ()
  "Move marked files to '/pdf' safely."
  (interactive)
  (let* ((target-dir "../pdf/")
         (expanded-target (expand-file-name target-dir))
         (files (dired-get-marked-files)))
    (unless (file-exists-p expanded-target)
      (make-directory expanded-target t))
    (condition-case nil
        (progn
          ;; dired-do-rename-regexp는 복잡할 수 있으므로 
          ;; 단순히 마킹된 파일을 옮기려면 dired-do-rename을 고려
          (dired-do-rename-regexp ".*" expanded-target nil t)
          (revert-buffer)
          (message "%d files moved to %s." (length files) target-dir))
      (quit (message "Move canceled."))
      (error (message "Error occurred during move.")))))

;; ======================================
;;; Dired Main Configuration
;; ======================================
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-free-space nil)
  :bind
  (:map dired-mode-map
   ("RET" . my-dired-open-dwim)
   ("C-<return>" . dired-do-open)
   ("M" . my-dired-move-to-pdf-folder-safe)
   ("/" . consult-line)))


(provide 'my-dired-custom)
;;; end my-dired-custom
