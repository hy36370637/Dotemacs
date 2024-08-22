;;; my-play-streaming.el --- Streaming and MP3 player for Emacs -*- lexical-binding: t; -*-
;; Ver 0.6

;;; Commentary:
;; This package provides streaming radio and MP3 player functionality for Emacs.
;; It includes VLC streaming capabilities and an MP3 player interface.
;; The MP3 player is designed for macOS and uses the 'afplay' command.

(require 'cl-lib)

;;; Streaming Radio
(defgroup my-streaming nil
  "Streaming radio settings."
  :group 'multimedia)

(defcustom my-streaming-mmslist-file
  (expand-file-name "~/.emacs.d/lisp/mmslist.txt")
  "Path to the mmslist.txt file."
  :type 'file
  :group 'my-streaming)

(defvar my-streaming-process nil
  "VLC process for streaming.")

(defvar my-streaming-playing nil
  "Flag indicating if streaming is currently playing.")

(defvar my-streaming-status ""
  "모드라인에 표시할 스트리밍 상태. 스트리밍 중이 아니면 공백.")

(defun my-streaming-update-status ()
  "스트리밍 라디오의 현재 상태에 따라 `my-streaming-status`를 업데이트합니다."
  (setq my-streaming-status
        (if my-streaming-playing
            (format "스트리밍: %s" (my-streaming-get-chosen-title my-streaming-mmslist-file))
          ""))
  (force-mode-line-update))

(defun my-streaming-toggle ()
  "Toggle streaming on/off."
  (interactive)
  (if my-streaming-playing
      (my-streaming-stop)
    (my-streaming-start (my-streaming-read-url-from-file my-streaming-mmslist-file))))

(defun my-streaming-start (url)
  "Start streaming audio from URL using VLC."
  (let* ((vlc-command (if (eq system-type 'darwin)
                          "/Applications/VLC.app/Contents/MacOS/VLC"
                        "vlc"))
         (chosen-title (my-streaming-get-chosen-title my-streaming-mmslist-file)))
    (if (not (executable-find vlc-command))
        (user-error "VLC is not installed")
      (unless my-streaming-process
        (setq my-streaming-process
              (start-process "vlc" nil vlc-command "--no-video" "-I" "rc" url))
        (set-process-query-on-exit-flag my-streaming-process nil)
        (setq my-streaming-playing t)
        (my-streaming-update-status)
        (message "Playing: %s" chosen-title)))))

(defun my-streaming-stop ()
  "Stop the currently running VLC process."
  (when my-streaming-process
    (delete-process my-streaming-process)
    (setq my-streaming-process nil
          my-streaming-playing nil)
    (my-streaming-update-status)
    (message "Streaming stopped.")))

(defun my-streaming-get-chosen-title (file)
  "Get the chosen title from the user using FILE."
  (let* ((items (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (titles (mapcar (lambda (item) (car (split-string item "|"))) items)))
    (completing-read "Choose a title to play: " titles)))

(defun my-streaming-read-url-from-file (file)
  "Read streaming URLs from FILE and return a URL chosen by the user."
  (let* ((items (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (titles (mapcar (lambda (item) (car (split-string item "|"))) items))
         (chosen-title (completing-read "Choose a title to play: " titles))
         (chosen-item (seq-find (lambda (item) (string-prefix-p chosen-title item)) items)))
    (when chosen-item
      (cadr (split-string chosen-item "|")))))

;;; MP3 Player
;; 기본 MP3 플레이어 설정
(defvar my-mp3-player-playlist nil
  "MP3 플레이어의 현재 재생 목록.")

(defvar my-mp3-player-index 0
  "MP3 플레이어의 현재 재생 트랙 인덱스.")

(defvar my-mp3-player-process nil
  "MP3 파일을 재생 중인 프로세스.")

(defvar my-mp3-player-status ""
  "모드라인에 표시할 MP3 플레이어 상태.")

(defvar my-mp3-player-default-directory "~/Dropbox/MP3"
  "기본 MP3 파일 디렉토리.")

(defvar my-mp3-player-buffer-name "*MP3 Player*"
  "MP3 플레이어 버퍼의 이름.")

(defvar my-mp3-player-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'my-mp3-player-play)
    (define-key map (kbd "s") 'my-mp3-player-stop)
    (define-key map (kbd "q") 'my-mp3-player-quit)
    map)
  "MP3 플레이어 인터페이스를 위한 키맵.")

(defun my-mp3-player-load-directory (directory)
  "사용자가 선택한 DIRECTORY에서 MP3 파일을 로드합니다."
  (interactive (list (read-directory-name "디렉토리 선택: " my-mp3-player-default-directory)))
  (setq my-mp3-player-playlist (directory-files directory t "\\.mp3$")
        my-mp3-player-index 0)
  (if my-mp3-player-playlist
      (message "총 %d개의 MP3 파일을 로드했습니다." (length my-mp3-player-playlist))
    (message "MP3 파일이 없습니다."))
  (my-mp3-player-update-buffer))

(defun my-mp3-player-play ()
  "MP3 파일을 재생합니다. 플레이리스트가 없으면 기본 디렉토리에서 파일을 로드합니다."
  (interactive)
  ;; 플레이리스트가 비어 있으면 디렉토리를 로드
  (unless my-mp3-player-playlist
    (my-mp3-player-load-directory my-mp3-player-default-directory))
  ;; 플레이리스트가 존재할 경우 재생 시작
  (if my-mp3-player-playlist
      (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
        (setq my-mp3-player-process 
              (start-process "afplay" nil "afplay" (expand-file-name file)))
        (set-process-sentinel my-mp3-player-process #'my-mp3-player-sentinel)
        (message "재생 중: %s" (file-name-nondirectory file))
        (my-mp3-player-update-status))
    (message "MP3 파일이 로드되지 않았습니다.")))

(defun my-mp3-player-stop ()
  "현재 재생 중인 MP3 파일을 중지합니다."
  (interactive)
  (when (and my-mp3-player-process (process-live-p my-mp3-player-process))
    (kill-process my-mp3-player-process)
    (setq my-mp3-player-process nil)
    (message "재생 중지됨.")
    (my-mp3-player-update-status)))

(defun my-mp3-player-sentinel (process event)
  "MP3 파일 재생이 종료되면 호출됩니다."
  (when (memq (process-status process) '(exit signal))
    (setq my-mp3-player-process nil)
    (message "MP3 파일 재생이 종료되었습니다.")
    (my-mp3-player-update-status)))

(defun my-mp3-player-update-status ()
  "MP3 플레이어의 상태에 따라 모드라인에 표시할 내용을 업데이트합니다."
  (setq my-mp3-player-status
        (if my-mp3-player-process
            (format "MP3: 재생 중 [%s]" (file-name-nondirectory (nth my-mp3-player-index my-mp3-player-playlist)))
          ""))
  (force-mode-line-update))

(defun my-mp3-player-update-buffer ()
  "MP3 플레이어 버퍼를 갱신합니다."
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "MP3 플레이어 - %s\n\n" (if my-mp3-player-process "재생 중" "중지됨")))
        (when my-mp3-player-playlist
          (insert (format "현재 트랙: %s (%d/%d)\n\n" 
                          (file-name-nondirectory (nth my-mp3-player-index my-mp3-player-playlist))
                          (1+ my-mp3-player-index)
                          (length my-mp3-player-playlist)))
          (insert "플레이리스트:\n")
          (dotimes (i (length my-mp3-player-playlist))
            (let ((track (nth i my-mp3-player-playlist)))
              (insert (format "%s %d. %s\n"
                              (if (= i my-mp3-player-index) ">" " ")
                              (1+ i)
                              (file-name-nondirectory track))))))
        (use-local-map my-mp3-player-map)
        (setq buffer-read-only t)))))

(defun my-mp3-player ()
  "MP3 플레이어 인터페이스를 시작합니다."
  (interactive)
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (unless my-mp3-player-playlist
        (call-interactively #'my-mp3-player-load-directory))
      (my-mp3-player-update-buffer)
      (use-local-map my-mp3-player-map))
    (switch-to-buffer-other-window buf))
  (message "MP3 플레이어 시작됨. 'p'를 눌러 재생/일시 정지, 'q'를 눌러 종료하세요."))

(defun my-mp3-player-quit ()
  "MP3 플레이어를 종료합니다."
  (interactive)
  (my-mp3-player-stop)
  (kill-buffer my-mp3-player-buffer-name)
  (message "MP3 플레이어 종료됨."))


;;; ends here

(provide 'my-play-streaming)
