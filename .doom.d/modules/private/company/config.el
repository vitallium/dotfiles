;;; private/company/config.el -*- lexical-binding: t; -*-

(after! ivy
  (setq +ivy-project-search-engines '(rg)
        +ivy-buffer-preview t))

(after! company
  (setq company-minimum-prefix-length 2))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(load! "+hooks")
(load! "+bindings")
