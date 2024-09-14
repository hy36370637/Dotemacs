;;; -*- lexical-binding: t; -*-
;; ======================================
;;; dired
;; --------------------------------------
;; dired 관련 lisp 모음
;; /.emacs.d/lisp/my-dired-custom.el
;; directory 우선/한글파일명 정렬불가(macOS) → 해결(setenv "LC_COLLATE" "C")

(use-package dired
  :ensure nil
  :preface
  (defun sof/dired-sort ()
    "Dired 정렬, 디렉토리를 우선으로."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (set-buffer-modified-p nil))

  (defun my/dired-jump-to-top ()
    "Dired에서 맨 위로 이동."
    (interactive)
    (goto-char (point-min))
    (dired-next-line 2))

  (defun my/dired-jump-to-bottom ()
    "Dired에서 맨 아래로 이동."
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))

  :custom
  (insert-directory-program "gls")
  (dired-listing-switches "-alh")
  (dired-dwim-target t) 
  (dired-recursive-copies 'always) 
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)

  :bind (:map dired-mode-map
              ("M-<up>" . my/dired-jump-to-top)
              ("M-<down>" . my/dired-jump-to-bottom)
              ("C-<return>" . dired-do-open)
	      ("C-c C-o" . dired-open-in-finder)
              ("/" . dired-narrow))

  :hook (dired-after-readin . sof/dired-sort))

  ;; :config
  ;; (setenv "LC_COLLATE" "C"))

(use-package dired-narrow
  :ensure t
  :after dired)

;;  dired 버퍼 관리 단순화. 새 디렉토리로 이동할 때마다 새 버퍼 생성 대신, 하나의 버퍼 재사용
(use-package dired-single
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("f" . dired-single-buffer)
              ("b" . dired-single-up-directory)))

(defun dired-open-in-finder ()
    "Open current directory in macOS Finder."
    (interactive)
    (shell-command (concat "open " (dired-current-directory))))


;; end here
(provide 'my-dired-custom)
