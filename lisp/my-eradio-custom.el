;;; my-eradio-custom.el --- Radio and media player configuration -*- lexical-binding: t; -*-
;;; ver0.5
;;; Code:

;; ======================================
;;; 1. Radio Helper Functions
;; ======================================
(defun load-eradio-channels-from-file (file-path)
  (unless (file-exists-p file-path)
    (error "Channel file not found: %s" file-path))
  (with-temp-buffer
    (insert-file-contents file-path)
    (let (channels)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
          (unless (string-empty-p line)
            (pcase-let ((`(,name ,url)
                         (mapcar #'string-trim (split-string line "|"))))
              (when (and name url (not (string-empty-p name)) (not (string-empty-p url)))
                (push (cons name url) channels))))
          (forward-line 1)))
      (nreverse channels))))

(defun my-eradio-reload-channels ()
  (interactive)
  (if-let ((channels (load-eradio-channels-from-file (expand-file-name "mmslist" my/lisp-path))))
      (setq eradio-channels channels)
    (warn "Reload failed")))

(defun my-openfile-mmslist ()
  (interactive)
  (find-file (expand-file-name "mmslist" my/lisp-path)))

;; ======================================
;;; 2. Unified Media Control
;; ======================================
(defun my-media-stop-all ()
  "라디오와 로컬 음악을 모두 정지합니다."
  (interactive)
  (my-music-stop)
  (when (fboundp 'eradio-stop)
    (eradio-stop))
  (message "⏹️ 모든 재생이 중단되었습니다."))

;; ======================================
;;; 3. VLC & eradio Configuration
;; ======================================
(defvar my-vlc-executable
  (or (and (boundp 'my-macOS-p) my-macOS-p
           (or (executable-find "/Applications/VLC.app/Contents/MacOS/VLC")
               (executable-find "vlc")))
      (executable-find "vlc")
      "vlc"))

(use-package eradio
  :bind (("C-c m p" . eradio-play)
         ("C-c m s" . my-media-stop-all)
         ("C-c m t" . eradio-toggle)
         ("C-c m d" . my-music-play-directory)
         ("C-c m f" . my-music-play-file)
         ("C-c m n" . my-music-next)
         ("C-c m b" . my-music-prev)
         ("C-c m SPC" . my-music-pause-toggle)
         ("C-c m r" . my-eradio-reload-channels)
         ("C-c m o" . my-openfile-mmslist))
  :custom
  (eradio-player (list my-vlc-executable "--no-video" "-I" "rc"))
  :config
  (setq eradio-channels (load-eradio-channels-from-file (expand-file-name "mmslist" my/lisp-path)))
  
  (advice-add 'eradio-play :before (lambda (&rest _) (my-music-stop)))   ;; 상호 정지 Advice
  (advice-add 'eradio-play :after                                        ;; 방송 정보 출력 Advice
              (lambda (&rest _)
                (when (boundp 'eradio-current-channel)
                  (let* ((current eradio-current-channel)
                         (name (if (consp current) (car current)
                                 (car (rassoc current eradio-channels))))
                         (display-name (if (and name (string-match "^[^. ]*\\.\\([^|]+\\)" name))
                                            (match-string 1 name)
                                          (or name "Unknown"))))
                    (message "🎶 Playing: %s" display-name))))))

;; ======================================
;;; 4. Local Music Engine (VLC)
;; ======================================
(defvar my-music-root (expand-file-name "~/Dropbox/MP3/"))
(defvar my-music-extensions '("mp3" "m4a" "flac" "wav" "ogg" "opus" "aac"))
(defvar my-music--files-cache nil)
(defvar my-music--process nil)
(defvar my-music--current-index 0)
(defvar my-music--current-playlist nil)
(defvar my-music--paused nil)

(defun my-music-refresh-cache ()
  (let ((regex (concat "\\.\\(" (mapconcat #'identity my-music-extensions "\\|") "\\)$")))
    (setq my-music--files-cache (sort (directory-files-recursively my-music-root regex) #'string<))))

(defun my-music-stop ()
  (interactive)
  (when (and (boundp 'my-music--process) my-music--process (process-live-p my-music--process))
    (kill-process my-music--process)
    (setq my-music--process nil my-music--paused nil)))

(defun my-music--start-vlc (files start-index &optional shuffle loop)
  (when (fboundp 'eradio-stop) (eradio-stop))
  (my-music-stop)
  (let* ((playlist-file (make-temp-file "vlc-playlist-" nil ".m3u"))
         (ordered-files (if shuffle (seq-shuffle (copy-sequence files)) files)))
    (with-temp-file playlist-file (dolist (f ordered-files) (insert f "\n")))
    (setq my-music--process
          (apply #'start-process "my-music-vlc" nil my-vlc-executable
                 (append (list "--no-video" "-I" "rc" "--rc-fake-tty")
                         (when loop '("--loop")) (list playlist-file))))
    (setq my-music--current-playlist ordered-files my-music--current-index start-index my-music--paused nil)
    (set-process-query-on-exit-flag my-music--process nil)
    (message "🎵 Playing: %s" (file-relative-name (nth start-index ordered-files) my-music-root))
    my-music--process))

(defun my-music-next ()
  (interactive)
  (when (and my-music--process (process-live-p my-music--process))
    (process-send-string my-music--process "next\n")
    (setq my-music--current-index (mod (1+ my-music--current-index) (length my-music--current-playlist)))))

(defun my-music-prev ()
  (interactive)
  (when (and my-music--process (process-live-p my-music--process))
    (process-send-string my-music--process "prev\n")
    (setq my-music--current-index (mod (1- my-music--current-index) (length my-music--current-playlist)))))

(defun my-music-pause-toggle ()
  (interactive)
  (when (and my-music--process (process-live-p my-music--process))
    (process-send-string my-music--process "pause\n")
    (setq my-music--paused (not my-music--paused))))

(defun my-music-play-file ()
  (interactive)
  (my-music-refresh-cache)
  (let* ((choices (mapcar (lambda (p) (cons (file-relative-name p my-music-root) p)) my-music--files-cache))
         (choice (completing-read "Play file: " (mapcar #'car choices) nil t))
         (full (cdr (assoc choice choices))))
    (when full (my-music--start-vlc (list full) 0 nil nil))))

(defun my-music-play-directory (prefix)
  (interactive "P")
  (my-music-refresh-cache)
  (when my-music--files-cache (my-music--start-vlc my-music--files-cache 0 prefix t)))

(provide 'my-eradio-custom)
;;; my-eradio-custom.el ends here
