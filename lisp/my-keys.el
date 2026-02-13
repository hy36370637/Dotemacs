;;; my-keys.el --- Optimized keybindings -*- lexical-binding: t; -*-

;; ======================================
;;; Helper Functions
;; ======================================
(defmacro my/defkeymap (name map-name &rest bindings)
  "Define keymap with NAME and BINDINGS, automatically setting up which-key labels."
  (declare (indent 2))
  `(progn
     (defvar-keymap ,name
       :name ,map-name
       ,@(mapcan (lambda (binding)
                   (list (car binding) (caddr binding)))
                 bindings))
     (which-key-add-keymap-based-replacements ,name
       ,@(mapcan (lambda (binding)
                   (list (car binding) (cadr binding)))
                 bindings))))


(defun my-prefix-with-ime-deactivation ()
  "Deactivate IME and show master keymap."
  (interactive)
  (my/deactivate-input-method)
  (which-key-show-keymap 'my-emacs-prefix-map my-emacs-prefix-map)
  (set-transient-map my-emacs-prefix-map t))

;; ======================================
;;; Keymap Definitions
;; ======================================
(my/defkeymap my-edit-prefix-map "Edit"
  ("i" "Indent dwim"      #'my-simple-indent-dwim)
  ("r" "Regexp replace"   #'my-query-replace-regexp-dwim)
  ("c" "Current line"     #'my-select-current-line)
  ("d" "Duplicate"        #'my-duplicate-dwim)
  ;; ("D" "Today stamp"      #'my-today-stamp)
  ("w" "Pairs wrap"       #'my-pair-pairs-wrap)
  ("%" "Replace"          #'query-replace))

(my/defkeymap my-search-prefix-map "Search"
  ("g" "Grep"             #'consult-grep)
  ("l" "Line"             #'consult-line)
  ("o" "Outline"          #'consult-outline)
  ("u" "Unified search"   #'my-search-unified)
  ("m" "Imenu"            #'consult-imenu)
  ("w" "Weather"          #'my-search-weather))

(my/defkeymap my-media-prefix-map "Media"
  ("P" "Play radio"       #'my-radio-play)
  ("S" "Stop radio"       #'my-radio-stop)
  ("i" "Insert img"       #'my-org-insert-image)
  ("p" "Path img"         #'my-insert-image-path)
  ("s" "Screenshot"       #'my-org-screenshot))

(my/defkeymap my-window-prefix-map "Window"
  ("j" "Width 1/3-2/3"     #'my-toggle-window-split-ratio)
  ("i" "Height 1/3-2/3"    #'my-toggle-window-height-ratio)
  ("k" "Pin/Unpin"         #'my-toggle-window-dedicated)
  ("l" "3-Win Layout"      #'my-layout-3-windows-center-focus)
  ("m" "Split 3-Column"    #'my-split-window-three-column))
  
(my/defkeymap my-emacs-prefix-map "Master"
  ("d" "Today's"          #'my-todays-pop)
  ("e" "Edit"             my-edit-prefix-map)
  ("F" "Full"             #'toggle-frame-fullscreen)
  ("m" "Media"            my-media-prefix-map)
  ("r" "Register"         #'jump-to-register)
  ("s" "Search"           my-search-prefix-map)
  ("u" "Usearch"          #'my-search-unified)
  ("w" "Window"           my-window-prefix-map))

;; ======================================
;;; Global Binding
;; ======================================
(keymap-set global-map "M-SPC" #'my-prefix-with-ime-deactivation)




(provide 'my-keys)
;;; my-keys.el ends here
