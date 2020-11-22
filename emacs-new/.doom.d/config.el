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

(setq doom-theme 'dracula)

(setq org-directory "~/Nextcloud/org/")
(setq org-agenda-files
      '("~/Nextcloud/org/Agenda/GTD.org"
        "~/Nextcloud/org/Agenda/Events.org"))
(setq display-line-numbers-type 'relative)



;; Set the opacity
(set-frame-parameter (selected-frame) 'alpha '(95 . 60))
(add-to-list 'default-frame-alist '(alpha . (95 . 60)))


(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "target"))

(after! rustic
  (setq rustic-flycheck-clippy-params "--message-format=json")
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-server 'rust-analyzer))

(after! dired
  ;; Image previews in dired
  (global-set-key (kbd "C-x i") 'peep-dired)
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "MP4")))


(after! org
  (setq +org-capture-todo-file "Agenda/GTD.org")
  (setq +org-capture-contacts-file (concat org-directory "contacts.org")))

(after! org-capture
  ;<<prettify-capture>>
  (add-transient-hook! 'org-capture-select-template
    (setq org-capture-templates
          (doct `((,(format "%s\tOrg Roam" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "d"
                   :type plain
                   :template ("- tags :: %?"
                              "- source :: ")
                   :function (lambda () (org-roam--capture-get-point))
                   :head "#+TITLE: ${title}\n#+SETUPFILE: ~/Nextcloud/org/config/setup.conf\n"
                   :unnarrowed t
                   )
                  (,(format "%s\tNew Contact" (all-the-icons-material "contacts" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "c"
                   :file +org-capture-contacts-file
                   :type entry
                   :jump-to-captured t
                   :children ((,(format "%s\tArmy" (all-the-icons-material "add" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "a"
                               :headline "Army"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tFriends" (all-the-icons-material "work" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "r"
                               :headline "Friends"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tothers" (all-the-icons-material "work" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "o"
                               :headline "Others"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tSchool" (all-the-icons-material "work" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "s"
                               :headline "School"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tcompany" (all-the-icons-material "work" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "c"
                               :headline "Company"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tWork" (all-the-icons-material "work" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "w"
                                :headline "Work"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END"))
                             (,(format "%s\tFamily" (all-the-icons-material "group" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "f"
                               :headline "Family"
                               :template ("* %?"
                                          ":PROPERTIES:"
                                          ":END")))
                   )
                  (,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "t"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  (,(format "%s\tBookmark" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "b"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Bookmark"
                   :type entry
                   :template ("* %? :%{i-type}:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
                   :i-type "web"
                   )
                  (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "n"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a")
                   )
                  (,(format "%s\tUniversity" (all-the-icons-faicon "graduation-cap" :face 'all-the-icons-purple :v-adjust 0.01))
                   :keys "u"
                   :file +org-capture-todo-file
                   :headline "University"
                   :prepend t
                   :type entry
                   :children ((,(format "%s\tTest" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust 0.01))
                               :keys "t"
                               :template ("* TODO [#C] %? :uni:tests:"
                                          "SCHEDULED: %^{Test date:}T"
                                          "%i %a"))
                              (,(format "%s\tAssignment" (all-the-icons-material "library_books" :face 'all-the-icons-orange :v-adjust 0.01))
                               :keys "a"
                               :template ("* TODO [#B] %? :uni:assignments:"
                                          "DEADLINE: %^{Due date:}T"
                                          "%i %a"))
                              (,(format "%s\tMiscellaneous task" (all-the-icons-faicon "list" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "u"
                               :template ("* TODO [#C] %? :uni:"
                                          "%i %a"))))
                  (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
                   :keys "e"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %? :email:"
                              "%i %a"))
                  (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
                   :keys "i"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children ((,(format "%s\tWebpage" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "w"
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "a"
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "i"
                               :desc ""
                               :i-type "read:info"
                               )
                              (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
                               :keys "I"
                               :desc ""
                               :i-type "idea"
                               )))
                  (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                   :keys "k"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i")
                   :children ((,(format "%s\tGeneral Task" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "k"
                               :extra ""
                               )
                              (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
                               :keys "d"
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              (,(format "%s\tScheduled Task" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
                               :keys "s"
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
                   :keys "p"
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children ((,(format "%s\tProject-local todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "t"
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              (,(format "%s\tProject-local note" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "n"
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              (,(format "%s\tProject-local changelog" (all-the-icons-faicon "list" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCenteralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   ))))))

(defun my/org-roam--backlinks-list-with-content (file)
  (with-temp-buffer
    (if-let* ((backlinks (org-roam--get-backlinks file))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          (insert (format "\n\n* %d Backlinks\n"
                          (length backlinks)))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** [[file:%s][%s]]\n"
                              file-from
                              (org-roam--get-title-or-slug file-from)))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                  (insert "\n\n")))))))
    (buffer-string)))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

(after! org-roam-server
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(defun my/org-publish (a b c)
  (setq org-export-with-toc t)
  (org-html-publish-to-html a b c))

(after! ox-publish
  (setq org-publish-project-alist
        '(("roam-static"
           :base-directory "~/Nextcloud/org/roam/static"
           :base-extension "js\\|css\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Nextcloud/org/publish/roam/static"
           :recursive t
           :publishing-function org-publish-attachment)
          ("roam-notes"
           :base-directory "~/Nextcloud/org/roam/"
           :publishing-directory "~/Nextcloud/org/publish/roam/"
           ;;:completion-function to shell script to load to server
           :publishing-function org-html-publish-to-html)
          ("roam"
           :components ("roam-static" "roam-notes")))))
(after! message
  (setq message-kill-buffer-on-exit t)
  ;; change the directory to store the sent mail
  (setq message-directory "~/.mail/"))

(after! org-journal
  (setq org-journal-dir "~/Nextcloud/org/journal/"))


