;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

(setq
 ;; Fonts
 ;; Primary font to use
 doom-font (font-spec :family "MonoLisa" :size 16.0 :weight 'semi-light)
 ;; Non-monospace font
 doom-variable-pitch-font (font-spec :family "ETBembo" :size 14.0)
 ;; For big-font-mode
 doom-big-font (font-spec :family (if IS-MAC "Monaco" "mononoki") :size 20.0)
 ;; For unicode glyphs
 ;; (doom-unicode-font)
 ;; For `fixed-pitch-serif' face
 ;; (doom-serif-font)
 ;; Theme
 doom-theme 'doom-one
 ;; Setup the style of line numbers
 display-line-numbers-type 'relative
 window-resize-pixelwise t)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(after! treemacs
  (add-hook 'treemacs-mode #'treemacs-follow-mode)
  (setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors"))
