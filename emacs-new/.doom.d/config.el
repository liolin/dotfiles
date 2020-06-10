;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Olivier Lischer"
      user-mail-address "olivier.lischer@liolin.ch")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'dracula)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "/keybase/private/liolin/org/")
(setq org-agenda-files
      '("/keybase/private/liolin/org/Agenda/GTD.org"
        "/keybase/private/liolin/org/Agenda/Events.org"))
;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Set the opacity
(set-frame-parameter (selected-frame) 'alpha '(85 . 60))
(add-to-list 'default-frame-alist '(alpha . (85 . 60)))


(after! mu4e
  (setq! mu4e-maildir (expand-file-name "~/.mail/olivier.lischer@liolin.ch") ; the rest of the mu4e folders are RELATIVE to this one
         mu4e-get-mail-command "mbsync -a"
         mu4e-index-update-in-background t
         mu4e-compose-signature-auto-include t
         mu4e-use-fancy-chars t
         mu4e-view-show-addresses t
         mu4e-view-show-images t
         mu4e-compose-format-flowed t
         ;mu4e-compose-in-new-frame t
         mu4e-change-filenames-when-moving t ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
         mu4e-maildir-shortcuts
         '( ("/Inbox" . ?i)
            ("/Archive" . ?a)
            ("/Drafts" . ?d)
            ("/Trash" . ?t)
            ("/Sent" . ?s))

         ;; Message Formatting and sending
         message-send-mail-function 'smtpmail-send-it
         message-signature-file "~/.mailsignature"
         message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
         message-citation-line-function 'message-insert-formatted-citation-line
         message-kill-buffer-on-exit t

         ;; Org mu4e
         org-mu4e-convert-to-html t)
  (add-to-list 'mu4e-bookmarks
               '( :name "HSR"
                  :query "from:hsr.ch"
                  :key ?h)))

(set-email-account! "olivier.lischer@liolin.ch"
                    '((user-mail-address            . "olivier.lischer@liolin.ch")
                      (user-full-name               . "Olivier Lischer")
                      (smtpmail-smtp-server         . "asmtp.mail.hostpoint.ch")
                      (smtpmail-default-smtp-server . "asmtp.mail.hostpoint.ch")
                      (smtpmail-smtp-service        . 587)
                      (smtpmail-stream-type         . starttls)
                      (smtpmail-debug-info          . t)
                      (mu4e-drafts-folder           . "/Drafts")
                      (mu4e-refile-folder           . "/Archive")
                      (mu4e-sent-folder             . "/Sent")
                      (mu4e-trash-folder            . "/Trash")
                      (mu4e-update-interval         . 300)
                      ;(mu4e-sent-messages-behavior . 'delete)
                      )
                    nil)
