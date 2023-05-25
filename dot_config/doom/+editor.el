;;; $DOOMDIR/+editor.el -*- lexical-binding: t; -*-

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Activate bug-reference-prog-mode
(after! prog-mode
  (bug-reference-prog-mode))
