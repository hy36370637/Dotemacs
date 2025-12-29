;;; my-eradio-custom.el --- Radio and media player configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized configuration for eradio (internet radio) and mpv player

;;; Code:

;; ======================================
;;; Helper Functions
;; ======================================
(defun load-eradio-channels-from-file (file-path)
  "Load radio channel definitions from FILE-PATH.  
Format: NAME|URL (one channel per line)"
  (unless (file-exists-p file-path)  ;; ← 부정문으로 변경 (명확함)
    (error "Channel file not found: %s" file-path))  ;; ← warn 대신 error
  (with-temp-buffer
    (insert-file-contents file-path)
    (let (channels)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
          (unless (string-empty-p line)
            (pcase-let ((`(,name ,url)  ;; ← 정규식 대신 split-string + pcase-let
                         (mapcar #'string-trim (split-string line "|"))))  ;; ← 간결한 파싱
              (when (and name url (not (string-empty-p name)) (not (string-empty-p url)))
                (push (cons name url) channels))))
          (forward-line 1)))
      (nreverse channels))))

(defun my-eradio-reload-channels ()
  "Reload eradio channels from the configuration file."
  (interactive)
  (if-let ((channels (load-eradio-channels-from-file
                      (expand-file-name "mmslist" my/lisp-path))))
      (progn
        (setq eradio-channels channels)
        (message "Loaded %d radio channels" (length channels)))
    (warn "No channels loaded or file not found")))

(defun my-eradio-play-url ()
  "Play a radio stream from a URL entered by the user."
  (interactive)
  (when-let ((url (read-string "Enter stream URL: ")))
    (unless (string-empty-p url)
      (eradio-play url))))

(defun my-open-mmslist ()
  "Open the file named \"mmslist\" in `my/lisp-path'."
  (interactive)
  (let ((file (expand-file-name "mmslist" my/lisp-path)))
    (unless (file-directory-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (find-file file)))

;; ======================================
;;; eradio Configuration
;; ======================================
(use-package eradio
  :bind (("C-c e p" .   eradio-play)
         ("C-c e s" .   eradio-stop)
         ("C-c e t" .  eradio-toggle)
         ("C-c e r" .  my-eradio-reload-channels)
         ("C-c e u" .  my-eradio-play-url)
         ("C-c e o" .   my-open-mmslist))
  :custom
  ;; Use VLC on macOS, fallback to mpv on other systems
  (eradio-player
   (if (and (boundp 'my-macOS-p) my-macOS-p)
       '("/Applications/VLC.app/Contents/MacOS/VLC"
         "--no-video" "-I" "rc")
     '("mpv" "--no-video" "--no-audio-display")))
    :init
  (unless (boundp 'my/lisp-path)
    (error "eradio:  my/lisp-path is not defined"))
  :config
  ;; Load channels from file at startup
  (let ((channel-file (expand-file-name "mmslist" my/lisp-path)))
    (setq eradio-channels (load-eradio-channels-from-file channel-file)))
  ;; Display channel name after playing
    (advice-add 'eradio-play :after
              (lambda (&rest _)
                (when (boundp 'eradio-current-channel)
                  (let* ((current eradio-current-channel)
                         (name (if (consp current)
                                   (car current)
                                 (car (rassoc current eradio-channels))))
                         (display-name (if (and name (string-match "^[^.]*\\.\\([^|]+\\)" name))
                                            (match-string 1 name)
                                          (or name "Unknown"))))
                    (message "🎶 Playing: %s" display-name))))))

;; ======================================
;;; mpv Configuration
;; ======================================
(use-package mpv
  :after dired
  :custom
  (mpv-executable
   (or (and (boundp 'my-macOS-p) my-macOS-p
            (or (executable-find "/opt/homebrew/bin/mpv")
                (executable-find "/usr/local/bin/mpv")))
       (executable-find "mpv")
       "mpv"))
  :config
  ;; Verify mpv is available
  (unless (executable-find mpv-executable)
    (warn "mpv executable not found at: %s" mpv-executable)))

(provide 'my-eradio-custom)
;;; my-eradio-custom.el ends here
