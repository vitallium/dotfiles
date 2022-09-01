;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Theme and font settings
(setq doom-font (font-spec :family "MonoLisa" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 22)
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      ;; Do not use variable pitch font for treemacs.
      doom-themes-treemacs-enable-variable-pitch nil)

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)
