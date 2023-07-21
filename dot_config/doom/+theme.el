;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

;; In Emacs > 29, we can use Po Lu’s pixel-scroll-precision-mode to get a faster and better scrolling.
(when *is-emacs-29*
  (pixel-scroll-precision-mode))

(setq
 ;; Fonts
 ;; Primary font to use
 doom-font (font-spec :family "Berkeley Mono" :size 14.0)
 ;; Non-monospace font
 doom-variable-pitch-font (font-spec :family "iA Writer Duo S" :size 14.0)
 ;; For big-font-mode
 doom-big-font (font-spec :family (if IS-MAC "Monaco" "mononoki") :size 20.0)
 ;; For unicode glyphs
 ;; (doom-unicode-font)
 ;; For `fixed-pitch-serif' face
 ;; (doom-serif-font)
 ;; Theme
 doom-theme 'doom-one
 ;; Disable line numbers
 display-line-numbers-type nil
 doom-font-increment 1.0
 window-resize-pixelwise t)

(setq modus-themes-mode-line '(accented))

(after! doom-themes
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(after! treemacs
  (add-hook! 'treemacs-mode-hook #'treemacs-follow-mode)
  (setq doom-themes-treemacs-enable-variable-pitch nil))

(after! doom-modeline
  (setq doom-modeline-bar-width 4
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'relative-to-project))

(when IS-MAC
  (setq frame-title-format nil)
  (dolist (filter '((ns-transparent-titlebar . t)
                    (ns-appearance . unbound)))
    (cl-pushnew filter default-frame-alist :test #'equal)))
