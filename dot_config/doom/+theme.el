;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

;; In Emacs > 29, we can use Po Lu’s pixel-scroll-precision-mode to get a faster and better scrolling.
(when *is-emacs-29*
  (pixel-scroll-precision-mode))

(setq
 ;; Fonts
 ;; Primary font to use
 doom-font (font-spec :family "Berkeley Mono Variable" :size 14.0)
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
 ;; Disable line numbers
 display-line-numbers-type nil
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

(use-package! auto-dark
  :custom
  (auto-dark-dark-theme 'doom-one)
  (auto-dark-light-theme 'doom-one-light)
  :config
  (auto-dark-mode t))

(when IS-MAC
  (setq frame-title-format nil)
  (dolist (filter '((ns-transparent-titlebar . t)
                    (ns-appearance . unbound)))
    (cl-pushnew filter default-frame-alist :test #'equal)))
