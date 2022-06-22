;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Theme and font settings
(setq doom-font (font-spec :family "MonoLisa" :size 21)
      doom-variable-pitch-font (font-spec :family "MonoLisa" :size 21)
      doom-theme 'modus-operandi
      display-line-numbers-type nil)

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

;; Dim inactive windows
(use-package! dimmer
  :config
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-mode t))

;; Popups
(set-popup-rule! "^\\*info.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*Man.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*tldr\\*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*helpful.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Help.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^ \\*Metahelp.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Apropos.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom :select t :quit t :ttl nil)
