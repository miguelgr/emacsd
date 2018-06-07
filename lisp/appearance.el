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

(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
;; Font size
(custom-set-faces
 '(default ((t (:height 100 :family "Fira Code" :weight normal)))))

(provide 'appearance)
