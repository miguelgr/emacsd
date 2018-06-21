;; Main configuraton for visual appearance
;; Themes
;; (add-to-list 'load-path "~/.emacs.d/themes/")
;; (load-theme 'rebecca 1)
;; (load-theme 'spacemacs-dark 1)
;; (load-theme 'jazz-theme 1)
;; (load-theme 'doom-nord 1)
(load-theme 'atom-one-dark 1)

;; Highlight parenthesis
(electric-pair-mode 1)
(show-paren-mode t)
(electric-indent-mode 1)
;; Hide: hidescrollbar, toolbar and menu
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Cursor
(blink-cursor-mode -1)
;; Disable bell
(setq ring-bell-function 'ignore
      visible-bell -1)
;; No splash screen please ... jeez
(setq inhibit-startup-message t
      backup-inhibited t)
;; Line
(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)
;; Quick feedback of pressed keys
(setq echo-keystrokes 0.1)
;; Modeline config
(display-time-mode)
(display-battery-mode 0)

 (custom-set-faces
    '(default ((t (:height 130 :family "Fantasque Sans Mono" :weight normal)))))

(provide 'appearance)
