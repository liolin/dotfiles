(setq user-full-name "Olivier Lischer"
      user-mail-address "olivier.lischer@liolin.ch")

(add-to-list 'load-path "~/.emacs.d/src/")
(setq exec-path (append '("~/.cargo/bin" "~/.rbenv/bin" "~/.local/bin") exec-path))

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(if (equal (locate-file "atom-one-dark-theme.el" custom-theme-load-path '("" "c")) nil)
    (progn
      (package-refresh-contents)
      (package-install 'dracula-theme)))
(load-theme 'atom-one-dark)

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'atom-one-dark)
  (add-hook 'after-init-hook 'sml/setup))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package neotree
    :ensure t
    :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package rainbow-delimiters
  :ensure t)

(setq indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq visible-cursor nil)

(tool-bar-mode -1)
(menu-bar-mode -1)

(linum-mode t)

(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(defun fish ()
  (interactive)
  (ansi-term "/usr/bin/fish" "fish"))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-M-m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))

(use-package flycheck
  :ensure t)

(use-package flycheck-inline
  :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-inline-mode)

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package smartparens
  :ensure t)

(require 'smartparens-config)

(use-package eglot
  :ensure t)

(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>")  'hs-hide-block)
(global-set-key (kbd "C-c <up>")    'hs-hide-all)
(global-set-key (kbd "C-c <down>")  'hs-show-all)

(use-package imenu-list
  :ensure
  :config (setq imenu-list-size 0.15)
  :bind ("C-'" . imenu-list-smart-toggle))

(use-package imenu-anywhere
  :bind ("C-." . helm-imenu-anywhere))

(use-package projectile
  :ensure
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init (add-hook 'c-mode-hook 'projectile-mode)
  (setq projectile-completion-system 'helm)
  :config (helm-projectile-on))

(use-package e2wm
  :ensure
  :init (global-set-key (kbd "M-+") 'e2wm:start-management))

(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t)

(use-package racer
  :ensure t
  :init
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path
	"/home/liolin/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))

(use-package flycheck-rust
  :ensure t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'smartparens-mode)
(add-hook 'rust-mode-hook 'hs-minor-mode)
(add-hook 'rust-mode-hook 'projectile-mode)
(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c <tab>") 'rust-format-buffer)))
(add-hook 'rust-mode-hook
	  (lambda ()
	    (flycheck-mode)
	    (flycheck-list-errors)))

(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)


(define-key rust-mode-map (kbd "C-c <tab>") 'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'company-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(add-hook 'js-mode-hook 'eglot-ensure)
(company-mode)

(use-package org
  :ensure t
  :init)

(setq org-hide-leading-stars t)

(setq org-drawers
      (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "NOTICE")))

(setq org-tags-exclude-from-inheritance '("ATTACH"))

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CANCELD")))
(setq org-default-priority 67)
(setq org-lowest-priority 69)

(setq org-log-done
(quote time))

(if (eq system-type 'windows-nt)
    (setq org-agenda-files
	  '("Z:/Privat/Org/Agenda/GTD.org"
	    "Z:/Privat/Org/Agenda/Events.org"))
  (setq org-agenda-files
	'("~/Nextcloud/Org/Agenda/GTD.org"
	  "~/Nextcloud/Org/Agenda/Events.org")))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-custom-commands
      '(("p" tags-todo "+project&+TODO=\"NEXT\"")
	("t" tags-todo "task")
	("c" "Agenda and Home-related tasks"
	 ((tags-todo "daily")
	  (tags-todo "monthly")
	  (tags-todo "+task")
	  (tags-todo "+TODO=\"NEXT\"")
	  (agenda "")))))

(setq org-src-fontify-natively t)

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
   '((emacs-lisp . t)
    (org . t)
    (python . t)
    (shell . t)
    (sql . t)
    (ledger . t)))

(use-package ox-reveal
  :ensure t
  :config
  (load-library "ox-reveal"))

(setq org-mobile-directory "~/Nextcloud/MobileOrg")
(setq org-directory "~/Nextcloud/Org")
(setq org-mobile-inbox-for-pull "~/Nextcloud/Org/flagged.org")

(setq org-structure-template-alist
      '(("n" . "notes")
	("a" . "export ascii")
	("c" . "center")
	("C" . "comment")
	("e" . "example")
	("E" . "export")
	("h" . "export html")
	("l" . "export latex")
	("q" . "quote")
	("s" . "src")
	("v" . "verse")))

(use-package deft
  :ensure t
  :init
  (if (eq system-type 'windows-nt)
      (setq deft-directory "Z:/Privat/Org/deft")
    (setq deft-directory "~/Nextcloud/Org/deft"))
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)
  :config
  (global-set-key (kbd "C-c d") 'deft))

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-buffers-list)
	 ("C-x r b" . helm-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 :map helm-map
	 ([tab] . helm-execute-persistent-action)))

(server-start)
