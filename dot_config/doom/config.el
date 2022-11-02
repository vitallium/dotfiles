;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      read-process-output-max (* 1024 1024)
      load-prefer-newer t
      scroll-margin 8
      projectile-project-search-path '("~/Development/"))

(load! "+ui")
(load! "+evil")
(load! "+prog")
(load! "+bindings.el")
(load! "+org.el")

(if IS-LINUX
  (load! "+mail.el")
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))

;; Convert images from unknown formats to PNG.
(setq image-use-external-converter t)

(after! ranger
  ;; Show hidden files in ranger windows.
  (setq ranger-show-hidden t))

(after! treemacs
  ;; Better git mode (requires Python 3).
  (setq treemacs-git-mode 'deferred))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
