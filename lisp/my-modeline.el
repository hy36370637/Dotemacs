;;; -*- lexical-binding: t; -*-
;; .emacs.d/lisp/my-modeline.el


;; =======================================
;;; Modeline
;; =======================================
(defvar my/indicator-image-dir 
  (expand-file-name "img-indicator/" user-emacs-directory))
(defvar ko-img 
  (create-image (expand-file-name "han2.tiff" my/indicator-image-dir) 
                'tiff nil :ascent 'center))
(defvar en-img 
  (create-image (expand-file-name "qwerty.tiff" my/indicator-image-dir) 
                'tiff nil :ascent 'center))
(defvar mode-line-use-images-p 
  (and (display-graphic-p) (image-type-available-p 'tiff)))
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e "
                mode-line-front-space
                (:eval
                 (let* ((is-ko (equal current-input-method "korean-hangul"))
                        (label (if is-ko "KO" "EN")))
                   (propertize label
                               'display (when mode-line-use-images-p 
                                          (if is-ko ko-img en-img))
                               'help-echo label)))
                "   "
                "Ⓗ "
                mode-line-buffer-identification
                mode-line-frame-identification
                "  "
                ;; mode-line-modes
                mode-line-format-right-align
                mode-line-position
                " Ⓨ "
                mode-line-misc-info))



(provide 'my-modeline)
;; end here
