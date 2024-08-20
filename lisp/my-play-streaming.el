;;; -*- lexical-binding: t; -*-
;; ======================================
;;; stream Radio
;; --------------------------------------
;; vlc streamming / Toggle On/Off (M-x toggle-streaming)
;; I was inspired by the eRadio package.
;; The streaming source is mmslist.txt, and it's in the format of title/address.
;; I'm using the Lubuntu distribution.
;; I used ChatGPT, and distribution is free.
;; /.emacs.d/lisp/my-play-streaming.el

(defvar stream-process nil
  "Variable to store the VLC process.")

(defvar stream-playing nil
  "Variable to track if streaming is currently playing.")

(defun get-mmslist-file-path ()
  "Get the file path for the mmslist.txt file."
  (if my-mactop-p
      "~/.emacs.d/lisp/mmslist.txt"))

(defun toggle-streaming ()
  "Toggle streaming on/off."
  (interactive)
  (if stream-playing
      (stop-streaming)
    (play-start-streaming (read-url-from-file (get-mmslist-file-path)))))

(defun play-start-streaming (url)
  "Start streaming audio from a given URL using VLC."
;;  (interactive "sURL: ")
  (let* ((vlc-command (if (eq system-type 'darwin) ;  ,
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

;; MP3 Player for Emacs
;; 이 플레이어는 macOS의 afplay 명령어를 사용하므로 macOS에서만 작동
;; MP3 파일만 지원합니다. 다른 형식의 오디오 파일은 TEST되지 않음
;; This package provides a simple MP3 player interface for Emacs.

(require 'cl-lib)

(defvar *my-mp3-player-playlist* nil "List of MP3 files to play.")
(defvar *my-mp3-player-index* 0 "Current index in the playlist.")
(defvar *my-mp3-player-process* nil "Current afplay process.")
(defvar *my-mp3-player-shuffle* nil "Whether shuffle mode is enabled.")
(defvar *my-mp3-player-repeat* nil "Whether repeat mode is enabled.")
(defvar *my-mp3-player-buffer-name* "*My MP3 Player*" "Name of the MP3 player buffer.")
(defvar *my-mp3-player-default-directory* "~/Dropbox/MP3" "Default directory for MP3 files.")

(defun my-mp3-player-load-directory (&optional directory)
  "Load MP3 files from the specified DIRECTORY or from the default directory."
  (interactive "DChoose directory: ")
  (setq directory (or directory *my-mp3-player-default-directory*))
  (setq *my-mp3-player-playlist* (directory-files directory t "\\.mp3$"))
  (setq *my-mp3-player-index* 0)
  (when *my-mp3-player-shuffle*
    (setq *my-mp3-player-playlist* (my-mp3-player-shuffle-list *my-mp3-player-playlist*)))
  (message "Loaded %d MP3 files from %s" (length *my-mp3-player-playlist*) directory)
  (my-mp3-player-update-buffer))

(defun my-mp3-player-play-pause ()
  "Play or pause the current MP3."
  (interactive)
  (if *my-mp3-player-process*
      (my-mp3-player-pause)
    (my-mp3-player-play)))

(defun my-mp3-player-play ()
  "Play the current MP3."
  (interactive)
  (when *my-mp3-player-playlist*
    (let ((file (nth *my-mp3-player-index* *my-mp3-player-playlist*)))
      (setq *my-mp3-player-process* 
            (start-process "afplay" nil "afplay" (expand-file-name file)))
      (set-process-sentinel *my-mp3-player-process* #'my-mp3-player-sentinel)
      (message "Now playing: %s" (file-name-nondirectory file))
      (my-mp3-player-update-buffer))))

(defun my-mp3-player-pause ()
  "Pause the current MP3."
  (interactive)
  (when *my-mp3-player-process*
    (interrupt-process *my-mp3-player-process*)
    (setq *my-mp3-player-process* nil)
    (message "Paused")
    (my-mp3-player-update-buffer)))

(defun my-mp3-player-stop ()
  "Stop playing MP3."
  (interactive)
  (when *my-mp3-player-process*
    (delete-process *my-mp3-player-process*)
    (setq *my-mp3-player-process* nil)
    (message "Stopped")
    (my-mp3-player-update-buffer)))

(defun my-mp3-player-next ()
  "Play the next MP3 in the playlist."
  (interactive)
  (my-mp3-player-stop)
  (setq *my-mp3-player-index* (mod (1+ *my-mp3-player-index*) (length *my-mp3-player-playlist*)))
  (my-mp3-player-play))

(defun my-mp3-player-previous ()
  "Play the previous MP3 in the playlist."
  (interactive)
  (my-mp3-player-stop)
  (setq *my-mp3-player-index* (mod (1- *my-mp3-player-index*) (length *my-mp3-player-playlist*)))
  (my-mp3-player-play))

(defun my-mp3-player-shuffle-toggle ()
  "Toggle shuffle mode."
  (interactive)
  (setq *my-mp3-player-shuffle* (not *my-mp3-player-shuffle*))
  (message "Shuffle mode toggled. Current state: %s" *my-mp3-player-shuffle*)
  (if *my-mp3-player-shuffle*
      (progn
        (message "Shuffling playlist...")
        (setq *my-mp3-player-playlist* (my-mp3-player-shuffle-list *my-mp3-player-playlist*))
        (setq *my-mp3-player-index* 0)
        (message "Playlist shuffled. New length: %d" (length *my-mp3-player-playlist*)))
    (progn
      (message "Sorting playlist...")
      (setq *my-mp3-player-playlist* (sort *my-mp3-player-playlist* #'string<))))
  (message "Shuffle mode: %s" (if *my-mp3-player-shuffle* "On" "Off"))
  (my-mp3-player-update-buffer))

(defun my-mp3-player-repeat-toggle ()
  "Toggle repeat mode."
  (interactive)
  (setq *my-mp3-player-repeat* (not *my-mp3-player-repeat*))
  (message "Repeat mode: %s" (if *my-mp3-player-repeat* "On" "Off"))
  (my-mp3-player-update-buffer))

(defun my-mp3-player-show-current ()
  "Show information about the current track."
  (interactive)
  (if *my-mp3-player-playlist*
      (let ((file (nth *my-mp3-player-index* *my-mp3-player-playlist*)))
        (message "Current track: %s (%d/%d)"
                 (file-name-nondirectory file)
                 (1+ *my-mp3-player-index*)
                 (length *my-mp3-player-playlist*)))
    (message "No playlist loaded")))

(defun my-mp3-player-sentinel (process event)
  "Handle the end of the MP3 playback."
  (when (string-match "finished" event)
    (if *my-mp3-player-repeat*
        (my-mp3-player-play)
      (my-mp3-player-next))))

(defun my-mp3-player-shuffle-list (list)
  "Return a shuffled copy of LIST."
  (let ((l (copy-sequence list)))
    (cl-loop for i from (1- (length l)) downto 1
             do (let* ((j (random (1+ i)))
                       (temp (nth i l)))
                  (setf (nth i l) (nth j l))
                  (setf (nth j l) temp)))
    l))

(defun my-mp3-player-update-buffer ()
  "Update the MP3 player buffer with current status."
  (let ((buf (get-buffer-create *my-mp3-player-buffer-name*)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "My MP3 Player\n\n")
      (insert (format "Shuffle: %s\n" (if *my-mp3-player-shuffle* "On" "Off")))
      (insert (format "Repeat: %s\n\n" (if *my-mp3-player-repeat* "On" "Off")))
      (if *my-mp3-player-playlist*
          (let ((file (nth *my-mp3-player-index* *my-mp3-player-playlist*)))
            (insert (format "Now Playing: %s\n" (file-name-nondirectory file)))
            (insert (format "Track: %d/%d\n\n" 
                            (1+ *my-mp3-player-index*) 
                            (length *my-mp3-player-playlist*)))
            (insert "Playlist:\n")
            (dotimes (i (length *my-mp3-player-playlist*))
              (let ((track (nth i *my-mp3-player-playlist*)))
                (insert (format "%s %d. %s\n"
                                (if (= i *my-mp3-player-index*) ">" " ")
                                (1+ i)
                                (file-name-nondirectory track))))))
        (insert "No playlist loaded\n"))
      (insert "\nControls:\n")
      (insert "p - Play/Pause\n")
      (insert "s - Stop\n")
      (insert "n - Next Track\n")
      (insert "b - Previous Track\n")
      (insert "r - Toggle Repeat\n")
      (insert "f - Toggle Shuffle\n")
      (insert "l - Load Directory\n")
      (insert "q - Quit\n"))
    (display-buffer buf)))

(defun my-mp3-player ()
  "Start the MP3 player interface."
  (interactive)
  (when (null *my-mp3-player-playlist*)
    (call-interactively #'my-mp3-player-load-directory))
  (my-mp3-player-update-buffer)
  (switch-to-buffer *my-mp3-player-buffer-name*)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'my-mp3-player-play-pause)
    (define-key map (kbd "s") 'my-mp3-player-stop)
    (define-key map (kbd "n") 'my-mp3-player-next)
    (define-key map (kbd "b") 'my-mp3-player-previous)
    (define-key map (kbd "r") 'my-mp3-player-repeat-toggle)
    (define-key map (kbd "f") 'my-mp3-player-shuffle-toggle)
    (define-key map (kbd "l") 'my-mp3-player-load-directory)
    (define-key map (kbd "q") 'quit-window)
    (use-local-map map))
  (message "My MP3 Player started. Press 'q' to quit."))


;; end here
(provide 'my-play-streaming)
