(after! flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-stylelint 'web-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(load! "+hooks")
