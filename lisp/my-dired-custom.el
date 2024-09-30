;;; -*- lexical-binding: t; -*-
;;;  /.emacs.d/lisp/my-dired-custom.el --- Custom Dired configuration
;;; Commentary:
;; directory 우선/한글파일명 정렬 관련 문제 해결: (setenv "LC_COLLATE" "C")
;;; Code:
(use-package dired
  :ensure nil
  :custom
  (insert-directory-program "gls")
  (dired-listing-switches "-alh")
  (dired-dwim-target t) 
  (dired-recursive-copies 'always) 
  (dired-recursive-deletes 'always)
  (dired-mouse-drag-files t)                  ; Emacs 29.1
  (dired-free-space nil)               	    ; Emacs 29.1
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  :bind (:map dired-mode-map
              ;; ("M-<up>" . my/dired-jump-to-top)
              ;; ("M-<down>" . my/dired-jump-to-bottom)
              ("C-<return>" . dired-do-open)
	      ;; ("C-c C-o" . dired-open-in-finder)
              ("/" . dired-narrow)
              ("M-p" . dired-mpv-play-file))  ; mpv 재생 단축키 추가
  :config
  (defun sof/dired-sort ()
    "Dired 정렬, 디렉토리를 우선으로."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (set-buffer-modified-p nil))

  ;; (defun my/dired-jump-to-top ()
  ;;   "Dired에서 맨 위로 이동."
  ;;   (interactive)
  ;;   (goto-char (point-min))
  ;;   (dired-next-line 2))

  ;; (defun my/dired-jump-to-bottom ()
  ;;   "Dired에서 맨 아래로 이동."
  ;;   (interactive)
  ;;   (goto-char (point-max))
  ;;   (dired-next-line -1))

  ;; (defun dired-open-in-finder ()
  ;;   "Open current directory in macOS Finder."
  ;;   (interactive)
  ;;   (shell-command (concat "open " (dired-current-directory))))

  (defun dired-mpv-play-file ()
    "Play the file at point with mpv."
    (interactive)
    (let ((file (dired-get-filename)))
      (start-process "mpv" nil mpv-executable file)))

  :hook ((dired-after-readin . sof/dired-sort)))
;;         (dired-mode . dired-hide-details-mode)))  ;Dired mode 상세정보 숨김

(use-package mpv
  :ensure t
  :after dired
  :config
  (setq mpv-executable "/opt/homebrew/bin/mpv"))

(use-package dired-narrow
  :ensure t
  :after dired)



(provide 'my-dired-custom)
;;; my-dired-custom.el ends here
