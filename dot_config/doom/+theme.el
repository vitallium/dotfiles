;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

(setq
      ;; Fonts
      ;; Primary font to use
      doom-font (font-spec :family "Berkeley Mono" :size 16)
      ;; Non-monospace font
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16)
      ;; For big-font-mode
      doom-big-font (font-spec :family (if IS-MAC "Monaco" "mononoki") :size 20)
      ;; For unicode glyphs
      ;; (doom-unicode-font)
      ;; For `fixed-pitch-serif' face
      ;; (doom-serif-font)
      ;; Theme
      doom-theme 'doom-one
      ;; Setup the style of line numbers
      display-line-numbers-type 'relative)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(after! treemacs
  (add-hook 'treemacs-mode #'treemacs-follow-mode))
