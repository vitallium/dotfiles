;;; $DOOMDIR/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! treesit-auto
  :if (and (require 'treesit)
           (treesit-available-p))
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
