;;; my-radio-direct.el --- Direct radio control via mpv -*- lexical-binding: t; -*-

;; ======================================
;;; 1. Variable
;; ======================================
(defvar my-radio-process-name "my-radio-mpv")
(defvar my-radio-mmslist (expand-file-name "mmslist" (or (bound-and-true-p my/lisp-path) user-emacs-directory)))

;; ======================================
;;; Main Function
;; ======================================
(defun my-radio-stop ()
  "Safely terminate the currently running radio process."
  (interactive)
  (if (get-process my-radio-process-name)
      (progn
        (delete-process my-radio-process-name)
        (message "⏹️ 라디오 재생 중지."))
    (message "재생 중인 라디오가 없습니다.")))

(defun my-radio-play ()
  "Select a radio channel from mmslist and start playback."
  (interactive)
  (if (not (file-exists-p my-radio-mmslist))
      (error "List file not found.: %s" my-radio-mmslist)
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
;;; Key-binding
;; ======================================
(defvar-keymap my-radio-prefix-map
  :doc "my-radio-prefix-map"
  :name "Radio"
  "p" #'my-radio-play
  "s" #'my-radio-stop)

;;; end
(provide 'my-radio-direct)
