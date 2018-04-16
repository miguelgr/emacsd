;;; emacsd --- Personal Emacs configuration
(when (version< emacs-version "25.1")
  (error "Configuration needs at least GNU Emacs 24.1. You are using %s" emacs-version))

;; Emacs server mode always on
;; emacsclient -t/-c
(server-mode)
(server-start)

;; Require packages
(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defmacro add-modes-hook (hook &rest modes)
  `(dolist (mode (quote ,modes))
     (let ((mode-name (symbol-name mode)))
       (add-hook (intern (format "%s-mode-hook" mode-name)) (quote ,hook)))))

(use-package resize-window
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-@" . er/expand-region)))

(use-package centered-window-mode
  :ensure t)

(use-package google-translate
  :ensure t
  :bind (("C-c / t" . google-translate-query-translate)
         ("C-c / ." . google-translate-at-point)))

(use-package google-this
  :ensure t
  :bind (("C-c / g" . google-this)))


(use-package howdoi
  :ensure t)

(use-package discover
  :ensure t
  :config (global-discover-mode 1))

(use-package discover-my-major
  :ensure t
  :bind (("C-c h" . discover-my-mayor)))

(use-package avy
  :ensure t
  :bind (("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package eldoc
  :diminish eldoc-mode
  :config (eldoc-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (ac-set-trigger-key "C-i"))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package flycheck-pyflakes
  :ensure t
  :config
  (add-hook 'python-mode-hook 'hungry-delete-mode))


(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:get-in-function-call-delay 700)
  :bind (("C-." . jedi:complete)
         ("C-c ." . jedi:goto-definition)
         ("C-c :" . jedi:goto-definition-next)))

;; go to ~/.docsets
(defvar basic-docsets '("Python 3" "Python 2" "Javascript" "Ansible"))

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-common-docsets basic-docsets
        helm-dash-browser-func 'eww)
  :bind (("s-/" . helm-dash-at-point)))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode))

(use-package zygospore
  :ensure t
  :demand t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-c m s" . magit-status)
	 ("C-c m b" . magit-blame)
	 ("C-c m f" . magit-diff-buffer-file)))

(use-package what-the-commit
  :bind ("C-x g c" . what-the-commit-insert))

(use-package github-browse-file
  :ensure t
  :bind ("C-x g b" . github-browse-file)
  :init (setq github-browse-file-show-line-at-point t))

(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode
  :init
  (add-hook 'js2-mode-hook 'hungry-delete-mode)
  (add-hook 'html-mode-hook 'hungry-delete-mode)
  (add-hook 'css-mode-hook 'hungry-delete-mode)
  (add-hook 'emacs-lisp-mode-hook 'hungry-delete-mode))

(use-package term
  :bind (:map
         term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map
         term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-S-b" . ibuffer)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-c i" . helm-imenu)
         ("C-x c !" . helm-calcul-expression)
         ("M-:" . helm-eval-expression-with-eldoc)

         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("C-h b" . helm-descbinds)
         ("C-h C-l" . helm-locate-library)
         ("C-c h" . helm-command-prefix))

    :config
    (setq helm-split-window-in-side-p t
          helm-buffers-fuzzy-matching t
          helm-buffer-max-length nil
          helm-recentf-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-ff-file-name-history-use-recentf t
          helm-ff-auto-update-initial-value t
          helm-full-frame nil)

    (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages))

(use-package helm-descbinds :ensure t :defer t)
(use-package helm-ag :ensure t :defer t)
(use-package resize-window :ensure t :defer t)
(use-package ag :ensure t :defer t)

(use-package helm-tramp :ensure t :defer t)
(use-package helm-themes :if (display-graphic-p) :bind ([f9] . helm-themes))

(use-package helm-projectile
  :ensure t
  :bind* (("C-c p D" . projectile-dired)
          ("C-c p v" . projectile-vc)
          ("C-c p k" . projectile-kill-buffers)

          ("C-c p p" . helm-projectile-switch-project)


          ("C-c p f" . helm-projectile-find-file)
          ("M-p" . helm-projectile-find-file)

          ("C-c p g" . helm-projectile-find-file-dwin)
          ("C-c p d" . helm-projectile-find-dir)
          ("C-c p C-r" . helm-projectile-recentf)
          ("C-c p b" . helm-projectile-switch-to-buffer)
          ("C-c p s s" . helm-projectile-ag)
          ("C-c p s g" . helm-projectile-grep))
  :diminish projectile-mode
  :init
  (setq-default projectile-enable-caching t
                projectile-indexing-method 'alien
                projectile-completion-system 'helm
                projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode)
  (helm-projectile-on))

(use-package wgrep
  :ensure t
  :bind (("C-c s" . wgrep-save-all-buffers))
  )

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package ace-isearch
  :pin melpa-stable
  :ensure t
  )

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-/" . undo-tree-undo)
         ("M-_" . undo-tree-redo)))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o h" . helm-info-org))
  :demand t
  :init
  (setq org-agenda-files '("~/Agenda/weeks/")
        org-src-fontify-natively t
        )
  (eval-after-load "org" '(require 'ox-md nil t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/packages/yasnippet-snippets/snippets"
                           "~/Projects/packages/yasnippet-django/snippets/models"))

  (yas-reload-all)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  )

(use-package howdoi
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )



;; (use-package neotree
;;   :ensure t
;;   :bind (([f8] . neotree-toggle))
;;   :init
;;   (setq neo-theme (if window-system 'icons 'arrow)
;;         neo-window-position 'right
;;         neo-smart-open t
;;         neo-window-width 30
;;         neo-window-fixed-size t
;;         neo-auto-indent-point t)

;;   (defun neotree-project-toggle ()
;;     "Open NeoTree using the git root."
;;     (interactive)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (if project-dir
;;           (if (neotree-toggle)
;;               (progn
;;                 (neotree-dir project-dir)
;;                 (neotree-find file-name)))
;;         (message "Could not find git project root."))))
;;   )

;; Initialize custom configuration
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editor)
(require 'python-dev)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; Major Modes

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :interpreter "clojure")

(use-package cider
  :ensure t
  :pin melpa-stable)

;; (use-package helm-cider
;;   :ensure t
;;   :config (helm-cider-mode))

;; (use-package aggressive-indent
;;   :ensure t
;;   :config
;;   (setq aggressive-indent-sit-for-time 0.5)
;;   (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq-default fill-column 80
                indent-tabs-mode nil)
  ;; Automatically remove trailing whitespace when file is saved.
  (add-hook 'python-mode-hook
            (lambda()
              (add-hook 'local-write-file-hooks
                        '(lambda()
                           (save-excursion
                             (delete-trailing-whitespace))))))

  :bind (("C-c C-b" . python-add-breakpoint))
  :interpreter "python")

(use-package pydoc
  :ensure t)

(use-package helm-pydoc
  :ensure t
  :bind (("C-c C-d" . helm-pydoc)))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :interpreter "go")

(use-package markdown-mode
  :ensure t
  :mode ("\\.md" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode "\\ya?ml\\'")

;; Themes

;;(use-package spacemas-theme :ensure t :defer t)
(use-package rebecca-theme :ensure t :defer t)
(use-package doom-themes :ensure t)
(use-package jazz-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(require 'appearance)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("9541f1dc11258239ef02aa1a5e9db3e1e46bc8fb1d7dbe83946c1541ae6dbdf9" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (spacemas-themes spacemas-theme fireplace ox-rst focus-mode ag spacemacs-theme spaceline google-this focus darktooth-theme beacon zygospore yasnippet yaml-mode xkcd window-numbering wgrep use-package undo-tree restclient resize-window rebecca-theme python-docstring pydoc pretty-symbols pretty-mode org-plus-contrib neotree multiple-cursors markdown-mode magit jedi jazz-theme hungry-delete howdoi hl-todo helm-tramp helm-pydoc helm-projectile helm-descbinds helm-dash helm-ag google-translate go-mode github-browse-file flycheck-pyflakes expand-region exec-path-from-shell efire doom-themes docker discover-my-major discover centered-window-mode atom-one-dark-theme ace-isearch))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Fira Code" :weight normal))))
 '(fringe ((t (:background "#292b2e")))))
