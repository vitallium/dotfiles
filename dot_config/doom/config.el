;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-theme 'doom-tokyo-night
      read-process-output-max (* 1024 1024)
      load-prefer-newer t
      scroll-margin 8
      display-line-numbers t
      display-line-numbers-type 'relative
      projectile-project-search-path '("~/Development/")
      ;; Set font settings
      doom-font                (font-spec :family "MonoLisa" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14)
      doom-serif-font          (font-spec :family "Iosevka Etoile" :size 15))

(load! "+theme.el")

(require 's)
(setenv "SSH_AUTH_SOCK" (s-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))

(setq +lookup-provider-url-alist
      '(("Google" "https://google.com/search?q=%s")
        ("GitHub" "https://github.com/search?ref=simplesearch&q=%s")
        ("Sourcegraph" "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")))

;; Activate bug-reference-prog-mode
(after! prog-mode
  (bug-reference-prog-mode))

(load! "+bindings.el")

(if (modulep! :completion company +tabnine)
  (load! "+tabnine.el"))

(if (modulep! :lang javascript)
  (load! "+javascript.el"))

(if (modulep! :lang ruby)
  (load! "+ruby.el"))

(if (modulep! :checkers spell +hunspell)
  (after! ispell
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US")
    (add-to-list 'ispell-local-dictionary-alist
                 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))))

(setq evil-ex-substitute-global t    ;; I like my s/../.. to be global by default
      evil-move-cursor-back nil      ;; Do not move the block cursor when toggling insert mode
      evil-kill-on-visual-paste nil  ;; Do not put overwritten text in the kill ring
      evil-vsplit-window-right t
      evil-split-window-below t)
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
