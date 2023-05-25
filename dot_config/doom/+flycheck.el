;;; $DOOMDIR/+flycheck.el -*- lexical-binding: t; -*-

;; Configure flycheck
(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-javascript-eslint-executable "eslint_d"
        flycheck-stylelintrc ".stylelintrc.json"
        flycheck-global-modes '(not org-mode)))
