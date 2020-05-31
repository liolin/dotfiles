(setq user-full-name "Olivier Lischer"
      user-mail-address "olivier.lischer@liolin.ch")

(add-hook 'before-save-hook 'whitespace-cleanup)

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

(setq indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq visible-cursor nil)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; check if the dracula theme installed. if not installed download and load it
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

(add-to-list 'load-path "~/.emacs.d/src/")

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (interactive)
  (let ((path-from-shell
	 (replace-regexp-in-string
	  "[[:space:]\n]*$" ""
	  (shell-command-to-string "bash -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; call function now
(set-exec-path-from-shell-PATH)

(if (eq system-type 'darwin)
    (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "de_CH")
  (setq ispell-local-dictionary-alist
	'(("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8))))

(global-hl-line-mode 1)
;;(set-face-background hl-line-face "#08510d")

(use-package dired
  :init
  (setq dired-recursive-deletes 'always))

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

(setq org-default-notes-file (concat org-directory "/capture.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n  %i\n  %a")))

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

(use-package ido
  :ensure t
  :init
  (ido-mode t))

(use-package flycheck
  :ensure t
  :config
  (defun my-flycheck ()
    (interactive)
    (flycheck-mode 1)
    (flycheck-list-errors))
  (setq flycheck-clang-args "-std=c++17"))

(defun my-hs ()
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t))

(defun my-linum ()
  (linum-mode t))

(use-package semantic
  :ensure t)
;; :init
;; (semantic-mode 1)
;; (defun my:add-semantic-to-autocompletion()
;;   (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocompletion)
;; (global-semantic-idle-scheduler-mode 1))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

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

(use-package irony
  :ensure t
  :init
  (defun liolin/irony-mode-on ()
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1)))

  (add-hook 'c++-mode-hook 'liolin/irony-mode-on)
  (add-hook 'c-mode-hook 'liolin/irony-mode-on)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package paren
  :ensure t
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode 1))

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  (setq ledger-reports
	'(("budget" "hledger -f %(ledger-file) bal -MB -b %(tagvalue) --budget cur:CHF expenses income")
	  ("bal" "%(binary) -f %(ledger-file) bal")
	  ("reg" "%(binary) -f %(ledger-file) reg")
	  ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
	  ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :mode ("\\.dat\\'"
	 "\\.hledger\\'"
	 "\\.journal\\'"))

(use-package php-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(setq c-default-style "bsd"
      c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (use-package auto-complete
;;              :ensure t
;;              :config
;;              (ac-config-default))

(use-package ac-php
  :ensure t)

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package php-auto-yasnippets
  :ensure t)

(use-package iedit
  :ensure t
  :init (define-key global-map (kbd "C-c ;") 'iedit-mode))

(use-package smartparens
  :ensure t
  :init (smartparens-global-mode 1))

(defun liolin/sp/open-block-c-mode (id action context)
  (when (eq action 'insert)
    (backward-char)
    (newline)
    (indent-according-to-mode)
    (forward-char)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(defun liolin/sp/open-block-php-mode (id action context)
  (when (eq action 'insert)
    (backward-char)
    (newline)
    (indent-according-to-mode)
    (forward-char)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(sp-local-pair 'c++-mode "{" nil :post-handlers '(:add liolin/sp/open-bloc-c-mode))
(sp-local-pair 'c-mode "{" nil :post-handlers '(:add liolin/sp/open-bloc-c-mode))

(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))

(use-package projectile
  :ensure
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init (add-hook 'c-mode-hook 'projectile-mode)
  (add-hook 'php-mode-hook 'projectile-mode)
  (setq projectile-completion-system 'helm)
  :config (helm-projectile-on))

(require 'epa-file)
(if (eq system-type 'darwin)
    (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg")))
(epa-file-enable)

(use-package helm-etags-plus
  :ensure t
  :config
  (global-set-key "\M-." 'helm-etags-plus-select))

(use-package helm-flycheck
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package company-php
  :ensure t)

;; (use-package company-irony
;;   :ensure t)

(use-package magit
  :ensure t)

(use-package phpunit
  :ensure t)

(use-package helm-phpunit
  :ensure t)

(use-package fixmee
  :ensure t)

(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

(global-set-key (kbd "C-c C-x b") 'browse-url-of-file)

(define-key global-map "\C-ca" 'org-agenda)

(if (eq system-type 'darwin)
    (setq path-to-ctags "/usr/local/ctags/bin/ctags")
  (setq path-to-ctags "c:/bin/ctags/ctags.exe"))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R --exclude='.ac-php-conf.json' --exclude='*.html' --exclude='*.css' --exclude='*.tpl' %s" path-to-ctags (directory-file-name dir-name))))

(global-set-key (kbd "M-*") 'pop-tag-mark)

(defun my-c-hook ()
  (my-hs)
  (my-linum)
  (my-flycheck))

(add-hook 'c-mode-common-hook 'my-c-hook)

(defun my-python-hook ()
  (my-hs)
  (my-linum)
  (my-flycheck))

(add-hook 'python-mode-hook 'my-python-hook)

(defun liolin/php-mode-hook ()
  (my-hs)
  (my-linum)
  (my-flycheck)

  ;; auto completion
  (require 'ac-php)
  (require 'company-php)
  (company-mode t)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend)
  (yas-global-mode 1)

  ;; if file not exists load from github
  (if (not (file-exists-p "~/.emacs.d/src/php-doc.el"))
      (url-copy-file "https://gist.githubusercontent.com/stesie/6564885/raw/f79cd412034a65ac95a7a25c27b1fadb5486f585/php-doc.el" "~/.emacs.d/src/php-doc.el"))
  (require 'php-doc)

  (ac-php-core-eldoc-setup ) ;; enable eldoc
  (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
  (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)    ;go back

  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq php-template-compatibility nil)
  (subword-mode 1))

(add-hook 'php-mode-hook 'liolin/php-mode-hook)
(add-hook 'php-mode-hook (lambda()
			   (add-hook 'after-save-hook 'ac-php-remake-tags nil 'nonNil)))

(defun my-web-mode-hook ()
  (local-set-key '[backtab] 'indent-relative)
  (setq indent-tabs-mode 1)
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(defun liolin/js-mode-hook ()
  (my-hs)
  (my-linum)

  ;; (auto-complete-mode t)
  (company-mode t)
  (yas-global-mode 1)

  (setq indent-tabs-mode 1)
  (setq c-basic-offset 4))

(add-hook 'js-mode-hook 'liolin/js-mode-hook)

(defun liolin/rust-hook ()
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (my-hs)
  (my-linum)
  (my-flycheck))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'liolin/rust-hook)
(add-hook 'rust-mode-hook #'projectile-mode)
(add-hook 'rust-mode-hook #'fixmee-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq racer-rust-src-path "~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-.") 'duplicate-line)

(defun desperately-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (compile "make -k"))))

(defun ox-reveal-workaround ()
  (interactive)
  (require 'org)
  (let ((current-prefix-arg 1))
    (call-interactively 'org-reload)))

(org-mobile-pull)
(find-file "~/Nextcloud/Org/flagged.org")
(org-agenda nil "c")

(server-start)
