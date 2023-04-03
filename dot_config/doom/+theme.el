;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(use-package! modus-themes
  :init
  (setq modus-themes-completions
      '((selection . (intense))
        (matches . (background intense)))
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-org-blocks 'gray-background)

  ;; I like the main modus-operandi colours in my git gutters.
  (custom-theme-set-faces! 'modus-operandi
    '(git-gutter-fr:deleted :background nil :foreground "#a60000" )
    '(git-gutter-fr:modified :background nil :foreground "#0031a9" )
    '(git-gutter-fr:added :background nil :foreground "#005e00")))

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)
