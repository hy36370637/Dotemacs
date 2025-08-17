;; early-init.el
;;;
;; 초기 GC 설정
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024 4))  ; 4MB로 증가

;; 파일 이름 핸들러 비활성화 (시작시에만)
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 시작 후 복원
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist my--file-name-handler-alist)))

;; 추가 최적화
(setq frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      auto-save-list-file-prefix nil)

;;;;; emacs 30.2 build err FIX START
;; Homebrew GCC 경로 설정 (Apple Silicon)
(let* ((gcc-bin "/opt/homebrew/opt/gcc/bin")
       (gcc-libcur "/opt/homebrew/opt/gcc/lib/gcc/current"))
  
  ;; PATH/exec-path 보강
  (when (file-directory-p gcc-bin)
    (setenv "PATH" (mapconcat #'identity
                              (delete-dups
                               (list gcc-bin
                                     "/opt/homebrew/bin"
                                     (getenv "PATH")))
                              ":"))
    (add-to-list 'exec-path gcc-bin))
  
  ;; libgccjit 설정
  (when (file-directory-p gcc-libcur)
    (setenv "LIBGCCJIT_EXEC_PREFIX" (concat gcc-libcur "/"))
    (setenv "LIBRARY_PATH" gcc-libcur)))
;;;;; emacs 30.2 build err FIX END

;; ======================================
;;; Emacs UI (초기 설정)
;; ======================================
(use-package emacs
  :init
  (setq inhibit-startup-message t
	visible-bell t
	initial-scratch-message nil
	use-dialog-box nil))

;;  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

;; ======================================
;;; Package initialization (기본 설정)
;; ======================================
(setq package-enable-at-startup nil)
