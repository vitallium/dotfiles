;;; checkers/jinx/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :init (global-jinx-mode)
  :custom
  (jinx-languages "en_US")
  (jinx-include-modes '(text-mode prog-mode git-commit-mode))
  :bind
  (("M-$" . jinx-correct)))
