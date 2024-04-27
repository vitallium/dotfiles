;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

;; Use Po Lu’s pixel-scroll-precision-mode to get a faster and better scrolling.
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

(after! modus-themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-prompts '(italic semibold)))

(after! ef-themes
  (setq ef-themes-to-toggle '(ef-day ef-night)))

(after! doom-themes
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(after! treemacs
  (add-hook! 'treemacs-mode-hook #'treemacs-follow-mode)
  (setq doom-themes-treemacs-enable-variable-pitch nil))

(setq
 ;; Fonts
 ;; Primary font to use
 doom-font (font-spec :family "MonoLisa" :size 14)
 ;; Non-monospace font
 doom-variable-pitch-font (font-spec :family "iA Writer Duo S" :size 14)
 ;; For big-font-mode
 doom-big-font (font-spec :family "MonoLisa" :size 20)
 ;; For unicode glyphs
 ;; (doom-unicode-font)
 ;; For `fixed-pitch-serif' face
 ;; (doom-serif-font)
 ;; Theme
 doom-theme 'modus-operandi
 ;; Disable line numbers
 display-line-numbers-type nil
 doom-font-increment 1.0
 window-resize-pixelwise t)

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

(after! doom-ui
  (setq! auto-dark-dark-theme 'modus-vivendi
         auto-dark-light-theme 'modus-operandi)
  (when (window-system) (auto-dark-mode t)))
