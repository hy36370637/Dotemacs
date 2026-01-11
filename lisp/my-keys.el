;;; my-keys.el --- Simplified keybindings -*- lexical-binding: t; -*-

;; Sub-Prefix Maps
(defvar-keymap my-capture-prefix-map
  :name "Capture"
  "c" #'org-capture                         ;; M-SPC c c : 전체 메뉴
  "d" (lambda () (interactive) (org-capture nil "d")) ;; M-SPC c d : Daily
  "t" (lambda () (interactive) (org-capture nil "t")) ;; M-SPC c Tasks
  "r" (lambda () (interactive) (org-capture nil "r")) ;; M-SPC c Reading
  "m" (lambda () (interactive) (org-capture nil "m"))) ;; M-SPC c 경조사

(defvar-keymap my-search-prefix-map
  :name "Search"
  "c" #'my-consult-ripgrep-selected-dir
  "f" #'consult-find
  "g" #'consult-grep
  "l" #'consult-line
  "o" #'consult-outline
  "m" #'consult-imenu
  "r" #'my-search-in-range
  "w" #'my-search-weather)

(defvar-keymap my-image-prefix-map
  :name "Image"
  "i" #'my-org-insert-image
  "p" #'my-insert-image-path
  "s" #'my-org-screenshot)

;; Master Keymap (Flattened & Simplified)
(defvar-keymap my-emacs-prefix-map
  :name "Master"
  "b" #'consult-bookmark
  "c" my-capture-prefix-map
  "C" #'my-capture-cReading-access
  "i" my-image-prefix-map
  "r" #'jump-to-register
  "s" my-search-prefix-map
  "t" #'my-todays-pop
  "w" #'my-pair-pairs-wrap
  "p" #'my-radio-play 
  "k" #'my-radio-stop
  "9" #'toggle-frame-maximized
  "0" #'toggle-frame-fullscreen)

;; which-key Labels
(which-key-add-keymap-based-replacements my-capture-prefix-map
  "c" "Capture Menu"
  "d" "Daily"
  "t" "Tasks"
  "r" "Reading"
  "m" "경조사")

(which-key-add-keymap-based-replacements my-search-prefix-map
  "c" "ripgrep"
  "f" "Find"
  "g" "Grep"
  "l" "Line"
  "o" "Outline"
  "m" "Imenu"
  "r" "sRange"
  "w" "Weather")

(which-key-add-keymap-based-replacements my-image-prefix-map
  "i" "Insert"
  "p" "Path"
  "s" "Screenshot")

(which-key-add-keymap-based-replacements my-emacs-prefix-map
  "b" "Bookmark"
  "c" "Capture"
  "C" "Reading"
  "i" "Images"
  "r" "Register"
  "s" "Search"
  "p" "Radio Play"
  "k" "Radio Stop"
  "t" "Today's"
  "w" "Pairs Wrap"
  "9" "Maximized"
  "0" "fullscreen")

(keymap-set global-map "M-SPC" my-emacs-prefix-map)

;; (with-eval-after-load 'embark
;;   ;; 1. 마스터 맵 전체를 embark-general-map에 연결
;;   ;; 이제 어떤 타겟 위에서 C-. 을 누르고 스페이스(또는 설정한 키)를 누르면
;;   ;; 사용자가 만든 모든 리더키 메뉴가 팝업됩니다.
;;   (define-key embark-general-map (kbd "SPC") my-emacs-prefix-map)

;;   ;; 2. 타겟이 없을 때 M-SPC와 똑같이 동작하도록 설정
;;   (setf (alist-get 't embark-keymap-alist) my-emacs-prefix-map))

(provide 'my-keys)
