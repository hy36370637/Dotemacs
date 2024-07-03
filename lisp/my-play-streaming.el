;;; -*- lexical-binding: t; -*-
;; ======================================
;;; stream Radio
;; --------------------------------------
;; vlc streamming / Toggle On/Off (M-x toggle-streaming)
;; I was inspired by the eRadio package.
;; The streaming source is mmslist.txt, and it's in the format of title/address.
;; I'm using the Lubuntu distribution.
;; I used ChatGPT, and distribution is free.
;; /emacs/lisp/my-play-streaming.el

(defvar stream-process nil
  "Variable to store the VLC process.")

(defvar stream-playing nil
  "Variable to track if streaming is currently playing.")

(defun get-mmslist-file-path ()
  "Get the file path for the mmslist.txt file."
  (if my-mactop-p
      "~/Dropbox/emacs/lisp/mmslist.txt"))

(defun toggle-streaming ()
  "Toggle streaming on/off."
  (interactive)
  (if stream-playing
      (stop-streaming)
    (play-start-streaming (read-url-from-file (get-mmslist-file-path)))))

(defun play-start-streaming (url)
  "Start streaming audio from a given URL using VLC."
;;  (interactive "sURL: ")
  (let* ((vlc-command (if (eq system-type 'darwin)
                          "/Applications/VLC.app/Contents/MacOS/VLC" ; for macOS
                        "vlc")))
    (if (not (executable-find vlc-command))
        (message "VLC is not installed. Please install VLC to stream audio.")
      (if (not stream-process)
          (let* ((chosen-title (get-chosen-title (get-mmslist-file-path))))
            (setq stream-process (start-process "vlc" nil vlc-command "--no-video" "-I" "rc" url))
            (set-process-query-on-exit-flag stream-process nil)
            (message "Playing: %s" chosen-title)
            (setq stream-playing t))
        (message "Streaming is already playing.")))))

(defun stop-streaming ()
  "Stop the currently running VLC process."
;;  (interactive)
  (if stream-process
      (progn
        (delete-process stream-process)
        (setq stream-process nil)
        (setq stream-playing nil)
        (message "Streaming stopped."))
    (message "No streaming is currently playing.")))

(defun get-chosen-title (file)
  "Get the chosen title from the user."
  (let* ((items (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (titles (mapcar (lambda (item) (car (split-string item "|"))) items))
         (chosen-title (completing-read "Choose a title to play: " titles)))
    chosen-title))

;; (defun edit-mmslist ()
;;   "Edit the mmslist.txt file."
;;   (interactive)
;;   (find-file (get-mmslist-file-path)))

(defun read-url-from-file (file)
  "Read streaming URLs from a file and return a URL chosen by the user."
  (let* ((items (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (titles (mapcar (lambda (item) (car (split-string item "|"))) items))
         (chosen-title (completing-read "Choose a title to play: " titles))
         (chosen-item (seq-find (lambda (item) (string= chosen-title (car (split-string item "|")))) items))
         (url (when chosen-item
                (cadr (split-string chosen-item "|")))))
    url))

(provide 'my-play-streaming)
