;;; private/mail/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(after! mu4e
  (setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
        ;; Refresh mail using isync every 10 minutes
        mu4e-update-interval (* 10 60)
        ;; This is set to 't' to avoid mail syncing issues when using mbsync
        mu4e-change-filenames-when-moving t
        mu4e-root-maildir "~/.mail")

  (setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "GitLab"
        :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "vslobodin@gitlab.com")))
        :vars '((user-mail-address      . "vslobodin@gitlab.com")
                (user-full-name         . "Vitaly Slobodin")
                (mu4e-sent-folder       . "/gitlab/[Gmail]/Sent Mail")
                (mu4e-drafts-folder     . "/gitlab/[Gmail]/Drafts")
                (mu4e-refile-folder     . "/gitlab/[Gmail]/All Mail")
                (mu4e-trash-folder      . "/gitlab/[Gmail]/Trash"))))))
