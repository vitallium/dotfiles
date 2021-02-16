;;; private/company/config.el -*- lexical-binding: t; -*-

(after! company
  (setq company-tooltip-limit 5
        company-tooltip-minimum-width 80
        company-tooltip-minimum 5
        company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

(setq +ivy-project-search-engines '(rg))

(load! "+hooks")
