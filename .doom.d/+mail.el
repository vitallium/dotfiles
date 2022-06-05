;;; $DOOMDIR/+mail.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(after! mu4e
  (setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
        ;; Refresh mail using isync every 10 minutes
        mu4e-update-interval (* 10 60)
        ;; This is set to 't' to avoid mail syncing issues when using mbsync
        mu4e-change-filenames-when-moving t
        mu4e-root-maildir "~/Mail"
        mu4e-html2text-command "w3m -T text/html"
        shr-use-colors nil))

(after! mu4e
  (set-email-account! "fastmail"
                      '((mu4e-sent-folder       . "/fastmail/Sent")
                        (mu4e-drafts-folder     . "/fastmail/Drafts")
                        (mu4e-trash-folder      . "/fastmail/Trash")
                        (mu4e-refile-folder     . "/fastmail/Archive")
                        (smtpmail-smtp-user     . "vitaly_slobodin@fastmail.com"))
                      t))

(after! mu4e
  (set-email-account! "gitlab"
                      '((mu4e-sent-folder       . "/gitlab/Sent")
                        (mu4e-drafts-folder     . "/gitlab/Drafts")
                        (mu4e-trash-folder      . "/gitlab/Trash")
                        (mu4e-refile-folder     . "/gitlab/Archive")
                        (smtpmail-smtp-user     . "vslobodin@gitlab.com"))
                      t)

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "GitLab Inbox"
                :query "maildir:\"/gitlab/Inbox\""
                :key ?i)))
