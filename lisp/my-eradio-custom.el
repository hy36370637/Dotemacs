;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-eradio-custom.el
;;

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

(defun my/eradio-toggle-hook ()
  "Configure eradio when the eradio-toggle command is first called."
  (interactive)
  (require 'eradio)
  (setq eradio-player '("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc"))
  ;;  (setq eradio-player '("mpv" "--no-video"))
  (setq eradio-channels (load-eradio-channels-from-file (concat my/lisp-path "mmslist.txt")))
  (eradio-toggle))

(autoload 'my/eradio-toggle-hook "my-eradio-custom" "Toggle eradio and configure it on first use." t)
(global-set-key (kbd "C-c SPC r") 'my/eradio-toggle-hook)


;; end here
(provide 'my-eradio-custom)
