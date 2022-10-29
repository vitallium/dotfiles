;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; Theme and font settings
(use-package! treemacs-all-the-icons
  :after treemacs)

(setq doom-font (font-spec :family "Source Code Pro" :size 22)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 24)
      doom-theme 'doom-tokyo-night
      display-line-numbers-type nil
      ;; Set the icons to be the same as in dired (all-the-icons)
      doom-themes-treemacs-theme "all-the-icons"
      treemacs-width-is-initially-locked nil
      mode-line-default-help-echo nil)

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Configure company
(use-package! company
  :config
  (setq company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend)))

(use-package! company-quickhelp
  :init
  (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay nil
        company-quickhelp-use-propertized-text t
        company-quickhelp-max-lines 10))

(use-package! dimmer
  :init
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode))

(put 'narrow-to-region 'disabled nil)

(def-modeline-var! +modeline-modes ; remove minor modes
  '(""
    mode-line-process
    "%n"))

(def-modeline! :main
  '(""
    +modeline-matches
    " "
    +modeline-buffer-identification
    +modeline-position)
  `(""
    mode-line-misc-info
    +modeline-modes
    "  "
    (+modeline-checker ("" +modeline-checker "    "))))

(set-modeline! :main 'default)
