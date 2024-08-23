;;; my-play-streaming.el --- Streaming and MP3 player for Emacs -*- lexical-binding: t; -*-
;; Version: 1.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia

;;; Commentary:
;; This package provides streaming radio and MP3 player functionality for Emacs.
;; It includes VLC streaming capabilities and an MP3 player interface.
;; The MP3 player is designed for macOS and uses the 'afplay' command.
;; Features a minor mode that integrates all MP3 player and streaming radio functions.

;;; Code:
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

(defconst my-mp3-player-buffer-name "*My Music Player*"
  "Name of the music player buffer.")

;;; Integrated Music Player Minor Mode
(defvar my-music-player-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m p") #'my-music-player-play-pause)
    (define-key map (kbd "C-c m s") #'my-music-player-stop)
    (define-key map (kbd "C-c m n") #'my-music-player-next)
    (define-key map (kbd "C-c m b") #'my-music-player-previous)
    (define-key map (kbd "C-c m r") #'my-music-player-repeat-toggle)
    (define-key map (kbd "C-c m f") #'my-music-player-shuffle-toggle)
    (define-key map (kbd "C-c m l") #'my-music-player-load-directory)
    (define-key map (kbd "C-c m i") #'my-music-player-show-current)
    (define-key map (kbd "C-c m v") #'my-music-player-view-playlist)
    (define-key map (kbd "C-c m t") #'my-music-player-toggle-streaming)
    (define-key map (kbd "C-c m q") #'my-music-player-quit)
    map)
  "Keymap for `my-music-player-mode'.")

(define-minor-mode my-music-player-mode
  "Minor mode for controlling the music player (MP3 and streaming)."
  :init-value nil
  :lighter " Music"
  :keymap my-music-player-mode-map
  (if my-music-player-mode
      (progn
        (my-music-player-initialize)
        (message "Music Player mode enabled. Use C-c m to access commands."))
    (my-music-player-cleanup)
    (message "Music Player mode disabled.")))

(defun my-music-player-initialize ()
  "Initialize the music player."
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (my-music-player-update-buffer)
      (use-local-map my-music-player-mode-map))
    (display-buffer buf)))

(defun my-music-player-cleanup ()
  "Clean up the music player resources."
  (my-music-player-stop)
  (when (get-buffer my-mp3-player-buffer-name)
    (kill-buffer my-mp3-player-buffer-name)))

(defun my-music-player-load-directory (directory)
  "Load MP3 files from DIRECTORY."
  (interactive (list (read-directory-name "Choose directory: "
                                          my-mp3-player-default-directory)))
  (setq my-mp3-player-playlist (directory-files directory t "\\.mp3$")
        my-mp3-player-index 0)
  (when my-mp3-player-shuffle
    (setq my-mp3-player-playlist (my-music-player-shuffle-list my-mp3-player-playlist)))
  (message "Loaded %d MP3 files from %s" (length my-mp3-player-playlist) directory)
  (my-music-player-update-buffer))

(defun my-music-player-play-pause ()
  "Play or pause the current music (MP3 or streaming)."
  (interactive)
  (if my-streaming-playing
      (my-streaming-toggle)
    (if my-mp3-player-process
        (my-music-player-pause)
      (my-music-player-play))))

(defun my-music-player-play ()
  "Play the current MP3."
  (interactive)
  (when my-mp3-player-playlist
    (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
      (setq my-mp3-player-process 
            (start-process "afplay" nil "afplay" (expand-file-name file)))
      (set-process-sentinel my-mp3-player-process #'my-music-player-sentinel)
      (message "Now playing: %s" (file-name-nondirectory file))
      (my-music-player-update-buffer))))

(defun my-music-player-pause ()
  "Pause the current MP3."
  (interactive)
  (when my-mp3-player-process
    (interrupt-process my-mp3-player-process)
    (setq my-mp3-player-process nil)
    (message "Paused")
    (my-music-player-update-buffer)))

(defun my-music-player-stop ()
  "Stop playing music (MP3 or streaming)."
  (interactive)
  (when my-mp3-player-process
    (delete-process my-mp3-player-process)
    (setq my-mp3-player-process nil)
    (message "Stopped")
    (my-music-player-update-buffer))
  (when my-streaming-playing
    (my-streaming-stop)))

(defun my-music-player-next ()
  "Play the next MP3 in the playlist."
  (interactive)
  (my-music-player-stop)
  (setq my-mp3-player-index (mod (1+ my-mp3-player-index) (length my-mp3-player-playlist)))
  (my-music-player-play))

(defun my-music-player-previous ()
  "Play the previous MP3 in the playlist."
  (interactive)
  (my-music-player-stop)
  (setq my-mp3-player-index (mod (1- my-mp3-player-index) (length my-mp3-player-playlist)))
  (my-music-player-play))

(defun my-music-player-shuffle-toggle ()
  "Toggle shuffle mode."
  (interactive)
  (setq my-mp3-player-shuffle (not my-mp3-player-shuffle))
  (if my-mp3-player-shuffle
      (setq my-mp3-player-playlist (my-music-player-shuffle-list my-mp3-player-playlist))
    (setq my-mp3-player-playlist (sort my-mp3-player-playlist #'string<)))
  (setq my-mp3-player-index 0)
  (message "Shuffle mode: %s" (if my-mp3-player-shuffle "On" "Off"))
  (my-music-player-update-buffer))

(defun my-music-player-repeat-toggle ()
  "Toggle repeat mode."
  (interactive)
  (setq my-mp3-player-repeat (not my-mp3-player-repeat))
  (message "Repeat mode: %s" (if my-mp3-player-repeat "On" "Off"))
  (my-music-player-update-buffer))

(defun my-music-player-show-current ()
  "Show information about the current track or stream."
  (interactive)
  (if my-streaming-playing
      (message "Currently streaming radio")
    (if my-mp3-player-playlist
        (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
          (message "Current track: %s (%d/%d)"
                   (file-name-nondirectory file)
                   (1+ my-mp3-player-index)
                   (length my-mp3-player-playlist)))
      (message "No playlist loaded"))))

(defun my-music-player-view-playlist ()
  "Display the current playlist in a separate buffer."
  (interactive)
  (if my-mp3-player-playlist
      (let ((buffer (get-buffer-create "*Music Playlist*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "Current Music Playlist:\n\n")
          (dotimes (i (length my-mp3-player-playlist))
            (let ((track (nth i my-mp3-player-playlist)))
              (insert (format "%s %3d. %s\n"
                              (if (= i my-mp3-player-index) ">" " ")
                              (1+ i)
                              (file-name-nondirectory track)))))
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buffer))
    (message "No playlist loaded.")))

(defun my-music-player-sentinel (process event)
  "Handle the end of the MP3 playback for PROCESS with EVENT."
  (when (string-match "finished" event)
    (if my-mp3-player-repeat
        (my-music-player-play)
      (my-music-player-next))))

(defun my-music-player-shuffle-list (list)
  "Return a shuffled copy of LIST."
  (let ((l (copy-sequence list)))
    (cl-loop for i from (1- (length l)) downto 1
             do (let* ((j (random (1+ i)))
                       (temp (nth i l)))
                  (setf (nth i l) (nth j l))
                  (setf (nth j l) temp)))
    l))

(defun my-music-player-update-buffer ()
  "Update the music player buffer with current status."
  (let ((buf (get-buffer-create my-mp3-player-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "My Music Player\n\n"
                (format "Shuffle: %s\n" (if my-mp3-player-shuffle "On" "Off"))
                (format "Repeat: %s\n\n" (if my-mp3-player-repeat "On" "Off")))
        (if my-streaming-playing
            (insert "Now Streaming Radio\n")
          (if my-mp3-player-playlist
              (let ((file (nth my-mp3-player-index my-mp3-player-playlist)))
                (insert (format "Now Playing: %s\n" (file-name-nondirectory file))
                        (format "Track: %d/%d\n\n" 
                                (1+ my-mp3-player-index) 
                                (length my-mp3-player-playlist))))
            (insert "No playlist loaded. Use C-c m l to load a directory.\n")))
        (insert "\nControls:\n"
                "C-c m p - Play/Pause\n"
                "C-c m s - Stop\n"
                "C-c m n - Next Track\n"
                "C-c m b - Previous Track\n"
                "C-c m r - Toggle Repeat\n"
                "C-c m f - Toggle Shuffle\n"
                "C-c m l - Load Directory\n"
                "C-c m i - Show Current Track\n"
                "C-c m v - View Playlist\n"
                "C-c m t - Toggle Streaming\n"
                "C-c m q - Quit\n")    
        (use-local-map my-music-player-mode-map)
        (setq buffer-read-only t)))))

(defun my-music-player-toggle-streaming ()
  "Toggle streaming radio on/off."
  (interactive)
  (my-streaming-toggle)
  (my-music-player-update-buffer))

(defun my-streaming-toggle ()
  "Toggle streaming on/off."
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

(defun my-music-player-quit ()
  "Quit the music player."
  (interactive)
  (my-music-player-mode -1))




;;; my-play-streaming.el ends here

(provide 'my-play-streaming)
