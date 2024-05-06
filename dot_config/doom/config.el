;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Always start Emacs window maximized
(add-hook! 'window-setup-hook #'toggle-frame-maximized)

;; Enable loading of local variables
(setq enable-local-variables :all)

(add-hook! 'prog-mode-hook #'global-mise-mode)

(load! "+personal")
(load! "+theme")
(load! "+bindings")
(load! "+editor")

(when (modulep! :editor evil)
  (load! "+evil"))

(when (modulep! :completion company)
  (load! "+company"))

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (load! "+lsp"))

(when (modulep! :term eshell)
  (load! "+eshell"))

(when (modulep! :term vterm)
  (load! "+vterm"))

(when (modulep! :lang ruby)
  (load! "+ruby"))

(when (modulep! :lang javascript)
  (load! "+javascript"))

(when (modulep! :lang org)
  (load! "+org"))

(when (modulep! :lang yaml)
  (load! "+yaml"))

(when (modulep! :lang markdown)
  (load! "+markdown"))

(when (modulep! :lang json)
  (load! "+json"))

;; Use GNU grep on macOS for faster `consult-grep`
;; `doom doctor` is still complaining, because of hard-coded `grep` command
(after! consult
  (if (featurep :system 'macos) (setq consult-grep-args (append '("ggrep") (cdr consult-grep-args) ))))
