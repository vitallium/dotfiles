;;; $DOOMDIR/tree-sitter.el -*- lexical-binding: t; -*-

(require 'treesit)

(use-package! treesit-auto
  :after treesit
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
