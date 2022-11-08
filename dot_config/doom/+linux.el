;;; $DOOMDIR/+linux.el -*- lexical-binding: t; -*-

(require 's)

(load! "+mail.el")
(setenv "SSH_AUTH_SOCK" (s-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
