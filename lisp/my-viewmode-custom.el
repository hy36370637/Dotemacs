;;; my-viewmode-custom.el --- configuration -*- lexical-binding: t; -*-

;; Enable read-only protection when entering view-mode
(use-package view
  :ensure nil
  :bind
  (:map view-mode-map
        ("n" . scroll-up-line)    ; 화면을 아래로 (텍스트를 위로)
        ("p" . scroll-down-line)  ; 화면을 위로 (텍스트를 아래로)
        ("e" . my-view-mode-edit-instantly))
  
  :hook (view-mode . my-view-mode-visual-setup) ; Hook 설정
  
  :config
  (setq view-read-only t))

(defun my-view-mode-visual-setup ()
  "view-mode 진입/해제 시 커서 모양과 줄 하이라이트만 변경합니다."
  (if view-mode
      (progn
        (hl-line-mode 1)
        ;; 읽기 모드일 때: 언더바(_) 모양 커서 (두께 3)
        (setq-local cursor-type '(hbar . 1)))
    (progn
      (hl-line-mode -1)
      ;; 편집 모드로 복귀 시: 원래 테마의 기본 커서(보통 box)로 복구
      (kill-local-variable 'cursor-type))))

(defun my-view-mode-edit-instantly ()
  "view-mode를 즉시 종료하고 편집 모드로 전환합니다."
  (interactive)
  (when view-mode
    (view-mode -1)
    (message "📝 편집 모드")))


(provide 'my-viewmode-custom)
;;; my-viewmode-custom.el ends here
