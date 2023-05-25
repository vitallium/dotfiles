;;; $DOOMDIR/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! treesit-auto
  :after treesit
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
