;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-eradio-custom.el

;; ======================================
;;; eradio
;; ======================================
(defun load-eradio-channels-from-file (file-path)
  "Load radio channel definitions from a file."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let (channels)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([^|]+\\)|\\(.*\\)$" line)
            (let ((name (match-string 1 line))
                  (url (match-string 2 line)))
              (push (cons name url) channels))))
        (forward-line 1))
      (nreverse channels))))

(use-package eradio
  :bind(("C-c e p" . eradio-play)
	("C-c e s" . eradio-stop)
	("C-c e t" . eradio-toggle))
:init
  (setq eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc"))
;;  (setq eradio-player '("mpv" "--no-video"))
  (setq eradio-channels (load-eradio-channels-from-file (concat my/lisp-path "mmslist.txt"))))

;; ======================================
;;; mpv
;; ======================================
(use-package mpv
  :ensure t
  :after dired
  :config
  (setq mpv-executable "/opt/homebrew/bin/mpv"))

;; end here
(provide 'my-eradio-custom)
