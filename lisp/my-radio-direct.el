;;; my-radio-direct.el --- Direct radio control via mpv -*- lexical-binding: t; -*-

;; ======================================
;;; 1. 설정 변수
;; ======================================
(defvar my-radio-process-name "my-radio-mpv")
(defvar my-radio-mmslist (expand-file-name "mmslist" (or (bound-and-true-p my/lisp-path) user-emacs-directory)))

;; ======================================
;;; 2. 핵심 제어 함수
;; ======================================

(defun my-radio-stop ()
  "현재 실행 중인 라디오 프로세스를 안전하게 종료합니다."
  (interactive)
  (if (get-process my-radio-process-name)
      (progn
        (delete-process my-radio-process-name)
        (message "⏹️ 라디오 재생 중지."))
    (message "재생 중인 라디오가 없습니다.")))

(defun my-radio-play ()
  "mmslist에서 채널을 선택하여 재생합니다."
  (interactive)
  (if (not (file-exists-p my-radio-mmslist))
      (error "목록 파일을 찾을 수 없습니다: %s" my-radio-mmslist)
    (let* ((channels (with-temp-buffer
                       (insert-file-contents my-radio-mmslist)
                       (let (res)
                         (goto-char (point-min))
                         (while (not (eobp))
                           (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                             ;; 빈 줄과 주석(;) 무시
                             (unless (or (string-empty-p line) (string-prefix-p ";" line))
                               (let ((parts (split-string line "|")))
                                 (when (>= (length parts) 2)
                                   (push (cons (string-trim (car parts)) (string-trim (cadr parts))) res)))))
                           (forward-line 1))
                         (nreverse res))))
           (selected (completing-read "📻 채널 선택: " (mapcar #'car channels) nil t))
           (url (cdr (assoc selected channels))))
      (when url
        (my-radio-stop) ;; 기존 프로세스 정리
        ;; mpv 실행 (비디오 없음, 터미널 없음, 메시지 억제)
        (start-process my-radio-process-name nil "mpv" "--no-video" "--no-terminal" "--msg-level=all=no" url)
        (message "🎶 %s 재생 시작..." selected)))))

;; ======================================
;;; 3. 키 바인딩 (전역)
;; ======================================
(global-set-key (kbd "C-c m p") #'my-radio-play)
(global-set-key (kbd "C-c m s") #'my-radio-stop)

(provide 'my-radio-direct)
