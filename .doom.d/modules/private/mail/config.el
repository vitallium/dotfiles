;;; private/mail/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(after! mu4e
  (setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
        ;; Refresh mail using isync every 10 minutes
        mu4e-update-interval (* 10 60)
        ;; This is set to 't' to avoid mail syncing issues when using mbsync
        mu4e-change-filenames-when-moving t
        mu4e-root-maildir "~/.mail"))


(set-email-account! "gitlab"
                    '(
                      (mu4e-sent-folder . "/gitlab/[Gmail]/Sent Mail")
                      (mu4e-drafts-folder . "/gitlab/[Gmail]/Drafts")
                      (mu4e-trash-folder . "/gitlab/[Gmail]/Trash")
                      ;; don't save message to Sent Messages, GMail/IMAP will take care of this
                      (mu4e-sent-messages-behavior 'delete))
                    t)
(after! mu4e
  (add-to-list 'mu4e-bookmarks
              (make-mu4e-bookmark
                :name "Inbox"
                :query "maildir:\"/gitlab/Inbox\""
                :key ?i)))

(use-package! mu4e-alert
  :after mu4e
  :init
  (setq doom-modeline-mu4e t)
  (mu4e-alert-set-default-style (if IS-MAC 'notifier 'notifications))
  (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:\"/gitlab/Inbox\"")
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))
