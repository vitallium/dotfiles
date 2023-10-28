;;; $DOOMDIR/+company.el -*- lexical-binding: t; -*-

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 3
        company-show-quick-access t
        company-tooltip-align-annotations t)
  (set-company-backend! '(prog-mode)  '(
                                        company-capf
                                        company-files
                                        company-yasnippet
                                        :separate
                                        company-tabnine
                                        )))

(use-package! company-quickhelp
  :after company
  :init
  (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay nil
        company-quickhelp-use-propertized-text t
        company-quickhelp-max-lines 10))
