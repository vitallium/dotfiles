(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(setq web-mode-engines-alist '(("vue" . "\\.vue\\'") ("elixir" . "\\.l?eex\\'")))

(after! flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-stylelint 'web-mode)
  (add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)))
  (add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)))
  (add-hook 'web-mode-hook (lambda () (doom/set-indent-width 2))))

(load! "+hooks")
