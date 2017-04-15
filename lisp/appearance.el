;; Main configuraton for visual appearance
(add-to-list 'load-path "~/.emacs.d/themes/")

(defvar my-font-family
  "Fantasque Sans Mono"
  "Preferred font family.")

(defvar my-font-size
  18
  "Preferred font size.")

;; Hide: hidescrollbar, toolbar and menu
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable bell
(setq ring-bell-function 'ignore)
(setq visible-bell -1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq backup-inhibited t)

;; Theme
(load-theme 'rebecca 1)

;; Cursor
(blink-cursor-mode -1)

;; Line
(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)


;; Quick feedback of pressed keys
(setq echo-keystrokes 0.2)

;; Modeline config
(display-time-mode)
(display-battery-mode)

;; Set fav font size
(set-face-attribute 'default nil :height 160)

;; Highlight parenthesis
(electric-pair-mode 1)
(show-paren-mode t)
(electric-indent-mode 1)
;;(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

(provide 'appearance)
