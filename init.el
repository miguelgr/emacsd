1;; Personal Emacs configuration
(when (version< emacs-version "25.1")
  (error "Configuration needs at least GNU Emacs 24.1. You are using %s" emacs-version))

;; Emacs server mode always on
;; emacsclient -t/-c
(server-mode)
(server-start)

;; Require packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package 'yasnippet))


(use-package expand-region
  :ensure t
  :bind (("C-@" . er/expand-region)))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package discover
  :ensure t
  :config (global-discover-mode 1))

(use-package linkd
  :load-path "packages/linkd"
  :config (linkd-mode))

(use-package eldoc
  :diminish eldoc-mode
  :config (eldoc-mode))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode))

(use-package zygospore
  :ensure t
  :demand t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))


(use-package magit
  :pin melpa-stable
  :ensure t
  :bind (("C-c m s" . magit-status)
	 ("C-c m b" . magit-blame)
	 ("C-c m f" . magit-diff-buffer-file)))

(use-package what-the-commit
  :bind ("C-x g c" . what-the-commit-insert))

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
          helm-full-frame nil
          helm-split-window-in-side-p t)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))

    (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages))

    ;; (my/enable-modes '(helm-mode
    ;;                    helm-descbinds-mode
    ;;                    helm-autoresize-mode
    ;;                    helm-flx-mode)))


(use-package helm-descbinds :ensure t :defer t)
(use-package helm-ag :ensure t :defer t)
(use-package helm-tramp :ensure t :defer t)
(use-package helm-themes :if (display-graphic-p) :bind ([f9] . helm-themes))
(use-package helm-swoop
  :ensure t
  :demand t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop)
         :isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)))

(use-package helm-projectile
  :ensure t
  :bind* (("C-c p D" . projectile-dired)
          ("C-c p v" . projectile-vc)
          ("C-c p k" . projectile-kill-buffers)

          ("C-c p p" . helm-projectile-switch-project)
          ("M-P" . helm-projectile-find-file)
          ("C-c p F" . helm-projectile-find-file-in-known-projects)
          ("C-c p g" . helm-projectile-find-file-dwin)
          ("C-c p d" . helm-projectile-find-dir)
          ("C-c p C-r" . helm-projectile-recentf)
          ("C-c p b" . helm-projectile-switch-to-buffer)
          ("C-c p s s" . helm-projectile-ag)
          ("C-c p s g" . helm-projectile-grep)
          )
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
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\ya?ml\\'")

(use-package yasnippet
  :commands (yas-minor-mode)
  :init ;; befe requiring the package
  (progn
    (add-hook 'python-mode-hook #'yas-minor-mode))
  :config ;  after requiring the package
  (progn
    (yas-reload-all)))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; Major Modes
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Themes
(use-package rebecca-theme :ensure t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package birds-of-paradise-plus-theme :ensure t :defer t)
(use-package bliss-theme :ensure t :defer t)
(use-package borland-blue-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)

;; Initialize custom configuration
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'appearance)
(require 'editor)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zygospore yaml-mode window-numbering what-the-commit web-mode use-package smart-mode-line restclient rebecca-theme org neotree multiple-cursors magit kosmos-theme hungry-delete helm-tramp helm-projectile helm-descbinds helm-ag expand-region discover cyberpunk-theme borland-blue-theme bliss-theme birds-of-paradise-plus-theme atom-one-dark-theme all-the-icons-dired ace-isearch))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
