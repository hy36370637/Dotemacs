;;; my-play-streaming.el --- Streaming and MP3 player for Emacs -*- lexical-binding: t; -*-

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
        (message "Playing: %s" chosen-title)))))

(defun my-streaming-stop ()
  "Stop the currently running VLC process."
  (when my-streaming-process
    (delete-process my-streaming-process)
    (setq my-streaming-process nil
          my-streaming-playing nil)
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
(defgroup my-mp3-player nil
  "MP3 player settings."
  :group 'multimedia)

(defcustom my-mp3-player-default-directory "~/Dropbox/MP3"
  "Default directory for MP3 files."
  :type 'directory
  :group 'my-mp3-player)

(defvar my-mp3-player-playlist nil
  "List of MP3 files to play.")

(defvar my-mp3-player-index 0
  "Current index in the playlist.")

(defvar my-mp3-player-process nil
  "Current afplay process.")

(defvar my-mp3-player-shuffle nil
  "Whether shuffle mode is enabled.")

(defvar my-mp3-player-repeat nil
  "Whether repeat mode is enabled.")

(defconst my-mp3-player-buffer-name "*My MP3 Player*"
  "Name of the MP3 player buffer.")

(defvar my-mp3-player-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'my-mp3-player-play-pause)
    (define-key map (kbd "s") #'my-mp3-player-stop)
    (define-key map (kbd "n") #'my-mp3-player-next)
    (define-key map (kbd "b") #'my-mp3-player-previous)
    (define-key map (kbd "r") #'my-mp3-player-repeat-toggle)
    (define-key map (kbd "f") #'my-mp3-player-shuffle-toggle)
    (define-key map (kbd "l") #'my-mp3-player-load-directory)
    (define-key map (kbd "q") #'my-mp3-player-quit)
    map)
  "Keymap for MP3 player commands.")

(defun my-mp3-player-load-directory (directory)
  "Load MP3 files from DIRECTORY."
  (interactive (list (read-directory-name "Choose directory: "
                                          my-mp3-player-default-directory)))
  (setq my-mp3-player-playlist (directory-files directory t "\\.mp3$")
        my-mp3-player-index 0)
  (when my-mp3-player-shuffle
    (setq my-mp3-player-playlist (my-mp3-player-shuffle-list my-mp3-player-playlist)))
  (message "Loaded %d MP3 files from %s" (length my-mp3-player-playlist) directory)
  (my-mp3-player-update-buffer))

(defun my-mp3-player-play-pause ()
  "Play or pause the current MP3."
  (interactive)
  (if my-mp3-player-process
      (my-mp3-player-pause)
    (my-mp3-player-play)))

(defun my-mp3-player-play ()
  "Play the current MP3."
  (interactive)
  (when my-mp3-player-playlist
    (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
      (setq my-mp3-player-process 
            (start-process "afplay" nil "afplay" (expand-file-name file)))
      (set-process-sentinel my-mp3-player-process #'my-mp3-player-sentinel)
      (message "Now playing: %s" (file-name-nondirectory file))
      (my-mp3-player-update-buffer))))

(defun my-mp3-player-pause ()
  "Pause the current MP3."
  (interactive)
  (when my-mp3-player-process
    (interrupt-process my-mp3-player-process)
    (setq my-mp3-player-process nil)
    (message "Paused")
    (my-mp3-player-update-buffer)))

(defun my-mp3-player-stop ()
  "Stop playing MP3."
  (interactive)
  (when my-mp3-player-process
    (delete-process my-mp3-player-process)
    (setq my-mp3-player-process nil)
    (message "Stopped")
    (my-mp3-player-update-buffer)))

(defun my-mp3-player-next ()
  "Play the next MP3 in the playlist."
  (interactive)
  (my-mp3-player-stop)
  (setq my-mp3-player-index (mod (1+ my-mp3-player-index) (length my-mp3-player-playlist)))
  (my-mp3-player-play))

(defun my-mp3-player-previous ()
  "Play the previous MP3 in the playlist."
  (interactive)
  (my-mp3-player-stop)
  (setq my-mp3-player-index (mod (1- my-mp3-player-index) (length my-mp3-player-playlist)))
  (my-mp3-player-play))

(defun my-mp3-player-shuffle-toggle ()
  "Toggle shuffle mode."
  (interactive)
  (setq my-mp3-player-shuffle (not my-mp3-player-shuffle))
  (if my-mp3-player-shuffle
      (setq my-mp3-player-playlist (my-mp3-player-shuffle-list my-mp3-player-playlist))
    (setq my-mp3-player-playlist (sort my-mp3-player-playlist #'string<)))
  (setq my-mp3-player-index 0)
  (message "Shuffle mode: %s" (if my-mp3-player-shuffle "On" "Off"))
  (my-mp3-player-update-buffer))

(defun my-mp3-player-repeat-toggle ()
  "Toggle repeat mode."
  (interactive)
  (setq my-mp3-player-repeat (not my-mp3-player-repeat))
  (message "Repeat mode: %s" (if my-mp3-player-repeat "On" "Off"))
  (my-mp3-player-update-buffer))

(defun my-mp3-player-show-current ()
  "Show information about the current track."
  (interactive)
  (if my-mp3-player-playlist
      (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
        (message "Current track: %s (%d/%d)"
                 (file-name-nondirectory file)
                 (1+ my-mp3-player-index)
                 (length my-mp3-player-playlist)))
    (message "No playlist loaded")))

(defun my-mp3-player-sentinel (process event)
  "Handle the end of the MP3 playback for PROCESS with EVENT."
  (when (string-match "finished" event)
    (if my-mp3-player-repeat
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
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "My MP3 Player\n\n"
                (format "Shuffle: %s\n" (if my-mp3-player-shuffle "On" "Off"))
                (format "Repeat: %s\n\n" (if my-mp3-player-repeat "On" "Off")))
        (if my-mp3-player-playlist
            (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
              (insert (format "Now Playing: %s\n" (file-name-nondirectory file))
                      (format "Track: %d/%d\n\n" 
                              (1+ my-mp3-player-index) 
                              (length my-mp3-player-playlist))
                      "Playlist:\n")
              (dotimes (i (length my-mp3-player-playlist))
                (let ((track (nth i my-mp3-player-playlist)))
                  (insert (format "%s %d. %s\n"
                                  (if (= i my-mp3-player-index) ">" " ")
                                  (1+ i)
                                  (file-name-nondirectory track))))))
          (insert "No playlist loaded\n"))
        (insert "\nControls:\n"
                "p - Play/Pause\n"
                "s - Stop\n"
                "n - Next Track\n"
                "b - Previous Track\n"
                "r - Toggle Repeat\n"
                "f - Toggle Shuffle\n"
                "l - Load Directory\n"
                "q - Quit\n")
        (use-local-map my-mp3-player-map)
        (setq buffer-read-only t)))))

(defun my-mp3-player-quit ()
  "Quit the MP3 player."
  (interactive)
  (my-mp3-player-stop)
  (kill-buffer my-mp3-player-buffer-name))

(defun my-mp3-player ()
  "Start the MP3 player interface."
  (interactive)
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (when (null my-mp3-player-playlist)
        (call-interactively #'my-mp3-player-load-directory))
      (my-mp3-player-update-buffer)
      (use-local-map my-mp3-player-map))
    (switch-to-buffer-other-window buf))
  (message "My MP3 Player started. Press 'p' to play/pause, 'q' to quit."))

;;; ends here

(provide 'my-play-streaming)
