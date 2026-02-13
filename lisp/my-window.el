;;; my-window.el --- Integrated Window and Frame Management -*- lexical-binding: t; -*-

;;; Commentary:
;; Manages isolated popup frame creation and automatic cleanup logic.
;; Supports Org-capture, Eshell, Dired, and Denote.

;;; Code:

;; ======================================
;;; Configurations
;; ======================================

(defvar my-window-popup-frame-parameters
  '((name . "Emacs-Popup")
    (width . 85)
    (height . 25)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (unsplittable . t))
  "Default frame parameters for new popup frames.")

(defvar my-window-popup-exit-configurations
  '((org-capture org-capture-after-finalize-hook)
    (esh-mode     eshell-exit-hook)
    ;; (dired        dired-mode-hook  (lambda () (local-set-key (kbd "q") #'my-window-delete-popup-frame)))
    ;; Denote: Close frame after saving the note buffer
    (denote       after-save-hook (lambda () 
                                    (when (and (fboundp 'denote-file-is-note-p)
                                               (denote-file-is-note-p buffer-file-name))
                                      (my-window-delete-popup-frame)))))
  "Registry for automatic popup closure. Format: (Feature Hook [Custom-Fn])")

;; ======================================
;;; Core Logic
;; ======================================
;;; ###autoload
(defmacro my-window-with-popup-frame (name-str &rest body)
  "Execute BODY within a new isolated popup frame.
Automatically deletes the frame upon completion or if an error/quit occurs."
  (declare (indent 1) (debug t))
  `(let ((frame (make-frame (append '((my-window-is-popup . t)) 
                                   my-window-popup-frame-parameters 
                                   (list (cons 'name (format "Popup: %s" ,name-str)))))))
     (select-frame frame)
     (condition-case err
         (progn ,@body)
       ((quit error user-error)
        (delete-frame frame)
        (signal (car err) (cdr err))))))

;;; ###autoload
(defun my-window-delete-popup-frame (&rest _)
  "Delete the current frame if it carries the `my-window-is-popup' parameter."
  (interactive)
  (when (frame-parameter nil 'my-window-is-popup)
    (delete-frame)))

;; ======================================
;;; Popup Commands
;; ======================================

;;; ###autoload
(defun my-window-popup-eshell ()
  "Launch an Eshell session in a dedicated popup frame."
  (interactive)
  (my-window-with-popup-frame "Eshell"
    (eshell)))

;;; ###autoload
;;(defun my-window-popup-dired (&optional dir)
;;  "Launch Dired in a dedicated popup frame."
;;  (interactive "DDirectory: ")
;;  (my-window-with-popup-frame "Dired"
;;    (dired (or dir default-directory))))

;;; ###autoload
(defun my-window-popup-denote ()
  "Create a new Denote note in a dedicated popup frame."
  (interactive)
  (my-window-with-popup-frame "Denote"
    (call-interactively #'denote)))

;;; ###autoload
(defun my-org-popup-capture ()
  "Launch `org-capture' in a dedicated popup frame.
Utilizes `my-window-with-popup-frame' for an isolated workflow.
Falls back to standard `org-capture' if the popup macro is unavailable."
  (interactive)
  (if (fboundp 'my-window-with-popup-frame)
      (my-window-with-popup-frame "Capture" (org-capture))
    (org-capture))) ; 만약 my-window 로드 실패 시 일반 capture 실행

;; ======================================
;;; Setup & Initialization
;; ======================================
(defun my-window-setup-popup-exits ()
  "Register exit hooks for various features defined in `my-window-popup-exit-configurations'."
  (dolist (config my-window-popup-exit-configurations)
    (let ((feature (nth 0 config))
          (hook    (nth 1 config))
          (custom  (nth 2 config)))
      (with-eval-after-load feature
        (if custom
            (add-hook hook custom)
          (add-hook hook #'my-window-delete-popup-frame))))))

(my-window-setup-popup-exits)

(provide 'my-window)
;;; my-window.el ends here
