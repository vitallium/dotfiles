;;; tools/just/config.el -*- lexical-binding: t; -*-

(use-package! just-mode
  :mode "\\.?justfile$")

(use-package! justl
  :config
  (map! :n "e" 'justl-exec-recipe))
