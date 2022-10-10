;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Theme and font settings
(setq doom-font (font-spec :family "MonoLisa" :size 19)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 24)
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      ;; Set the icons to be the same as in dired (all-the-icons)
      doom-themes-treemacs-theme "doom-colors"
      ;; Do not use variable pitch font for treemacs
      doom-themes-treemacs-enable-variable-pitch nil
      ;; use unicode as a fallback (instead of ASCII) when not using icons
      doom-modeline-unicode-fallback t
      ;; don't display the buffer encoding
      doom-modeline-buffer-encoding nil
      ;; I like a little padding for my modeline
      doom-modeline-height 30
      doom-modeline-buffer-file-name-style 'truncate-with-project
      highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'stack)

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package! dimmer
  :init
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode))
