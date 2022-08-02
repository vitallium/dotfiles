;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Theme and font settings
(setq doom-font (font-spec :family "Iosevka Comfy" :size 22)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 20)
      doom-theme 'doom-one
      display-line-numbers-type nil
      doom-themes-treemacs-theme "doom-colors"
      ;; Do not use variable pitch font for treemacs.
      doom-themes-treemacs-enable-variable-pitch nil)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Popups
(set-popup-rule! "^\\*info.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*Man.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*tldr\\*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*helpful.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Help.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^ \\*Metahelp.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Apropos.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom :select t :quit t :ttl nil)

;; Vterm
(after! vterm
  (setq vterm-buffer-name-string "vterm %s"))
