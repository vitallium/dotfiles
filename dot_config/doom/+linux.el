;;; $DOOMDIR/+linux.el -*- lexical-binding: t; -*-

(load! "+mail.el")
(setenv "SSH_AUTH_SOCK" (s-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
