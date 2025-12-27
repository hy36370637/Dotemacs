;;; my-eradio-custom.el --- Radio and media player configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized configuration for eradio (internet radio) and mpv player

;;; Code:

;; ======================================
;;; Helper Functions
;; ======================================

(defun load-eradio-channels-from-file (file-path)
  "Load radio channel definitions from FILE-PATH.
Format: NAME|URL (one channel per line, # for comments)"
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let (channels)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim 
                       (buffer-substring-no-properties 
                        (line-beginning-position) 
                        (line-end-position)))))
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (when (string-match "^\\([^|]+\\)|\\(.+\\)$" line)
                (let ((name (string-trim (match-string 1 line)))
                      (url (string-trim (match-string 2 line))))
                  (when (and (not (string-empty-p name))
                             (not (string-empty-p url)))
                    (push (cons name url) channels)))))
            (forward-line 1)))
        (nreverse channels)))))

(defun my-eradio-reload-channels ()
  "Reload eradio channels from the configuration file."
  (interactive)
  (if-let ((channels (load-eradio-channels-from-file 
                      (expand-file-name "mmslist.txt" my/lisp-path))))
      (progn
        (setq eradio-channels channels)
        (message "Loaded %d radio channels" (length channels)))
    (warn "No channels loaded or file not found")))

(defun my-eradio-play-url ()
  "Play a radio stream from a URL entered by the user."
  (interactive)
  (when-let ((url (read-string "Enter stream URL: ")))
    (unless (string-empty-p url)
      (eradio-play-channel (cons "Custom URL" url)))))

;; ======================================
;;; eradio Configuration
;; ======================================
(use-package eradio
  :bind
  (("C-c e p" . eradio-play)
   ("C-c e s" . eradio-stop)
   ("C-c e t" . eradio-toggle)
   ("C-c e r" . my-eradio-reload-channels)
   ("C-c e u" . my-eradio-play-url))
  
  :custom
  ;; Use VLC on macOS, fallback to mpv on other systems
  (eradio-player 
   (if (eq system-type 'darwin)
       '("/Applications/VLC.app/Contents/MacOS/VLC" 
         "--no-video" "-I" "rc")
     '("mpv" "--no-video" "--no-audio-display")))
  
  :config
  ;; Load channels from file
  (unless (boundp 'my/lisp-path)
    (error "eradio: my/lisp-path is not defined"))
  
  (let ((channel-file (expand-file-name "mmslist.txt" my/lisp-path)))
    (setq eradio-channels 
          (or (load-eradio-channels-from-file channel-file)
              (error "eradio: Channel file not found or invalid at %s" 
                     channel-file))))
  
  ;; Display channel name after playing
  (advice-add 'eradio-play :after
              (lambda (&rest _)
                "Show channel name after playing."
                (run-with-timer 
                 0.5 nil
                 (lambda ()
                   (when (and (boundp 'eradio-current-channel) 
                              eradio-current-channel)
                     (let ((name (if (consp eradio-current-channel)
                                     (car eradio-current-channel)
                                   (or (car (rassoc eradio-current-channel 
                                                    eradio-channels))
                                       eradio-current-channel))))
                       (message "▶ Playing: %s" name))))))))

;; ======================================
;;; mpv Configuration
;; ======================================

(use-package mpv
  :after dired
  
  :custom
  ;; Set mpv executable path based on system
  (mpv-executable 
   (cond
    ((eq system-type 'darwin)
     (or (executable-find "/opt/homebrew/bin/mpv")
         (executable-find "/usr/local/bin/mpv")
         "mpv"))
    ((eq system-type 'gnu/linux)
     (or (executable-find "mpv") "mpv"))
    (t "mpv")))
  
  :config
  ;; Verify mpv is available
  (unless (executable-find mpv-executable)
    (warn "mpv executable not found at: %s" mpv-executable)))


(provide 'my-eradio-custom)
;;; my-eradio-custom.el ends here
