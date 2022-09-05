;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

(use-package! modus-themes
  :init
  (modus-themes-load-themes))

(map! "<f5>" #'modus-themes-toggle)

;; Theme and font settings
(setq doom-font (font-spec :family "PragmataPro Mono Liga" :size 23)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 22)
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      ;; Do not use variable pitch font for treemacs.
      doom-themes-treemacs-enable-variable-pitch nil
      ;; use unicode as a fallback (instead of ASCII) when not using icons
      doom-modeline-unicode-fallback t
      ;; don't display the buffer encoding
      doom-modeline-buffer-encoding nil
      ;; I like a little padding for my modeline
      doom-modeline-height 40
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
  (setq dimmer-fraction 0.4)
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode))
