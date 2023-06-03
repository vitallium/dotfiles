;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Always start Emacs window maximized
(add-hook 'window-setup-hook 'toggle-frame-maximized)

(load! "+personal")
(load! "+theme")
(load! "+bindings")
(load! "+editor")

(when (modulep! :editor evil)
  (load! "+evil"))

(when (modulep! :tools lsp)
  (load! "+lsp"))

(when (modulep! :completion company)
  (load! "+company"))

(when (modulep! :term vterm)
  (load! "+vterm"))

;; FIXME: Conflicts with Doom's tree-sitter module
;; (when (modulep! :tools tree-sitter)
;;   (load! "+tree-sitter"))

(when (modulep! :lang ruby)
  (load! "+ruby"))

(when (modulep! :lang javascript)
  (load! "+javascript"))

(when (modulep! :lang org)
  (load! "+org"))

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
