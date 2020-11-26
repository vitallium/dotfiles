(setq auto-mode-alist (delete '("\\.js\\'" . js2-mode) auto-mode-alist))
(setq web-mode-engines-alist '(("vue" . "\\.vue\\'")))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(add-hook! js2-mode 'prettier-js-mode)
(add-hook 'vue-mode-hook #'lsp!)
(add-hook! js2-mode jest-minor-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)))
(add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)))
