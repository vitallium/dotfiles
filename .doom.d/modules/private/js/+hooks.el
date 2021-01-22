;; (setq auto-mode-alist (delete '("\\.js\\'" . js2-mode) auto-mode-alist))
(setq web-mode-engines-alist '(("vue" . "\\.vue\\'")))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))()

;; Prettier
(add-hook! js2-mode #'prettier-js-mode)
(add-hook! rjsx-mode-hook #'prettier-js-mode)

;; Vue.js
(add-hook! vue-mode-hook #'lsp!)
(add-hook! vue-mode-hook #'prettier-js-mode)
(after! vue-mode
  (setq mmm-submode-decoration-level 0))

;; Jest
(add-hook! js2-mode #'jest-minor-mode)

;; (add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)))
;; (add-hook 'web-mode-local-vars-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)))
