;; early-init.el
;; ======================================
;;; Speed up Emacs startup
;; ======================================
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 16))))  ; 16MB
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; ======================================
;;; Emacs UI (초기 설정)
;; ======================================
(use-package emacs
  :init
  (setq inhibit-startup-message t
	visible-bell t
	initial-scratch-message nil
	use-dialog-box nil)
  :config
;;  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; (setq inhibit-startup-message t
;;       visible-bell t
;;       initial-scratch-message nil
;;       use-dialog-box nil)

;; ;; 메뉴, 툴바, 스크롤바 숨기기
;; ;;(menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; ======================================
;;; Package initialization (기본 설정)
;; ======================================
(setq package-enable-at-startup nil)
