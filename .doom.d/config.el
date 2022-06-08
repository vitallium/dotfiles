;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-font (font-spec :family "Iosevka Comfy" :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 24)
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      read-process-output-max (* 1024 1024)
      load-prefer-newer t)

(load! "+evil")
(load! "+ui")
(load! "+prog")
(load! "+bindings.el")
(load! "+mail.el")
(load! "+org.el")

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
