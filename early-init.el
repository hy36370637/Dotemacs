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
