;;; my-eradio-custom.el --- Radio and media player configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for eradio (internet radio) and mpv player

;;; Code:

;; ======================================
;;; Helper Functions
;; ======================================
(defun load-eradio-channels-from-file (file-path)
  "Load radio channel definitions from FILE-PATH.
Format: NAME|URL (one channel per line)"
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let (channels)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties 
                     (line-beginning-position) 
                     (line-end-position))))
          (when (string-match "^\\([^|]+\\)|\\(.*\\)$" line)
            (push (cons (match-string 1 line) 
                        (match-string 2 line)) 
                  channels)))
        (forward-line 1))
      (nreverse channels))))

;; ======================================
;;; eradio
;; ======================================
(use-package eradio
  :bind
  (("C-c e p" . eradio-play)
   ("C-c e s" . eradio-stop)
   ("C-c e t" . eradio-toggle))
  :custom
  (eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" 
                   "--no-video" "-I" "rc"))   ;;  (setq eradio-player '("mpv" "--no-video"))
  :config
  (setq eradio-channels 
        (load-eradio-channels-from-file 
         (expand-file-name "mmslist.txt" my/lisp-path))))

;; ======================================
;;; mpv
;; ======================================
(use-package mpv
  :after dired
  :custom
  (mpv-executable "/opt/homebrew/bin/mpv"))

;;; my-eradio-custom.el ends here
(provide 'my-eradio-custom)
