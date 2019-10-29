;;; emacsd --- Personal Emacs configuration
(when (version< emacs-version "25.1")
  (error "Configuration needs at least GNU Emacs 24.1. You are using %s" emacs-version))

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

(use-package expand-region
  :ensure t
  :bind (("C-@" . er/expand-region)))

(use-package dired
  :config
  (setq dired-dwim-target t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-sidebar
  :ensure t
  :bind (("C-c d" . dired-sidebar-toggle-sidebar)))

;; Google services
(use-package google-translate
  :ensure t
  :bind (("C-c / t" . google-translate-query-translate)
         ("C-c / ." . google-translate-at-point)))

(use-package google-this
  :ensure t
  :bind (("C-c / g" . google-this)))

(use-package resize-window
  :ensure t)

(use-package avy
  :ensure t
  :bind (("M-g l" . avy-goto-line)
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
  (custom-set-variables
   '(flycheck-python-flake8-executable "python3")
   '(flycheck-python-pycompile-executable "python3")
   '(flycheck-python-pylint-executable "python3"))
  (add-hook 'python-mode-hook 'hungry-delete-mode))

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:environment-root "jedi")
  (setq jedi:get-in-function-call-delay 200)

  :bind (("C-." . jedi:complete)
         ("C-c ." . jedi:goto-definition)
         ("C-c :" . jedi:goto-definition-next)))

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
  :config
  (setq magit-section-initial-visibility-alist '((unpushed . show)))
  :bind (("C-c m s" . magit-status)
	 ("C-c m b" . magit-blame)
	 ("C-c m d" . magit-diff-buffer-file)
	 ("C-c m f" . magit-find-file-other-window)))

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
         ("C-h C-l" . helm-locate-library))


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

(use-package ag :ensure t :defer t)
(use-package helm-descbinds :ensure t :defer t)
(use-package helm-ag
 :ensure t
 :defer t
;; :config (setq helm-follow-mode-persistent t)
 :bind (("C-c a" . helm-do-grep-ag)
        ("C-c p s p" . helm-do-ag-project-root))
)

(use-package helm-projectile
  :ensure t
  :bind* (("C-c p D" . projectile-dired)
          ("C-c p k" . projectile-kill-buffers)
          ("C-c p p" . helm-projectile-switch-project)

          ("C-c p f" . helm-projectile-find-file)
          ("M-P" . helm-projectile-find-file)

          ("C-c p g" . helm-projectile-find-file-dwin)
          ("C-c p d" . helm-projectile-find-dir)
          ("C-c p C-r" . helm-projectile-recentf)
          ("C-c p i" . projectile-invalidate-cache)
          ("C-c p b" . helm-projectile-switch-to-buffer)
          ("C-c p s a" . helm-projectile-ag)
          ("C-c s" . helm-projectile-ag)
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
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets/snippets"))

  (yas-reload-all)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  )


(use-package buffer-move
  :ensure t
  :bind (("C-c w <up>"    . buf-move-up)
         ("C-c w <down>"  . buf-move-down)
         ("C-c w <left>"  . buf-move-left)
         ("C-c w <right>" . buf-move-right)))


(use-package zeal-at-point
  :ensure t
  :bind ("C-c d" . zeal-at-point))

;; Initialize custom configuration
;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editor)
(require 'python-dev)

;; Major Modes
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))


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
  (setq py-shell-name "python3")
  (setq-default fill-column 80
                indent-tabs-mode nil
                python-environment-directory "~/.emacs.d/.python-environments/src/")
  ;; Automatically remove trailing whitespace when file is saved.
  (add-hook 'python-mode-hook
            (lambda()
              (add-hook 'local-write-file-hooks
                        '(lambda()
                           (save-excursion
                             (delete-trailing-whitespace))))))

  :bind (("C-c C-b" . python-add-breakpoint))
  :interpreter "ipython")

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :interpreter "go")

(use-package go-playground
  :ensure t)

(use-package rust-mode
  :ensure t
  :config (setq rust-format-on-save t))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode)
  :mode ("\\.md" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml" . yaml-mode))

(use-package haml-mode
  :ensure t
  :mode ("\\.haml" . haml-mode))

;; Themes
(require 'appearance)

;; Emacs server mode always on
;; emacsclient -t/-c
(server-mode)
(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Fantasque Sans Mono" :weight normal)))))

