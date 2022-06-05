;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

(advice-add #'doom-modeline-segment--modals :override #'ignore)

(use-package doom-themes
  :custom
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Configure treemacs styling
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-completions '((t background intense accented))
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-variable-pitch-ui t
        modus-themes-org-agenda
        '((header-block . (variable-pitch scale-title))
          (header-date . (grayscale bold-all)))
        modus-themes-org-blocks
        '(grayscale)
        modus-themes-mode-line
        '(accented)
        modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120)
(setq visual-fill-column-width fill-column)

(setq visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Popups
;; * help
(set-popup-rule! "^\\*info.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*Man.*" :size 82 :side 'right :ttl t :select t :quit t)
(set-popup-rule! "^\\*tldr\\*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*helpful.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Help.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^ \\*Metahelp.*" :size 82 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Apropos.*" :size 82 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom :select t :quit t :ttl nil)

;; (after! magit
;;   (set-popup-rule! "^\\*Magit" :slot -1  :side 'right :size  80 :modeline nil :select t)
;;   (set-popup-rule! "^\\*Magit Repositories\\*" :side 'bottom :size 50 :modeline nil :select nil)
;;   (set-popup-rule! "^\\*magit.*popup\\*" :slot 0 :side 'right  :modeline nil :select  t)
;;   (set-popup-rule! "^\\*magit-gitflow.*popup\\*" :slot 0 :side 'bottom :size 0.25 :modeline nil :select  t)
;;   (set-popup-rule! "^\\*magit-revision:.*" :slot  0 :side  'right :window-height 0.6 :modeline  nil :select  t)
;;   (set-popup-rule! "^\\*magit-diff:.*" :slot  0 :side 'right :window-height  0.6 :modeline nil :select nil))
