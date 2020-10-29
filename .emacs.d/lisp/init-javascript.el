;;; init-javascript

(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'lsp)
  ; (add-hook 'web-mode-hook 'smartparens-mode)
  ; (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (setq typescript-indent-level 2)
  (setq typescript-fmt-on-save t)
  (setq typescript-fmt-tool 'prettier)
  (setq typescript-backend 'lsp))

;; JavaScript mode
(use-package js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
	;; Don't mishighlight shebang lines
	js2-skip-preprocessor-directives t
	;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
	;; Flycheck provides these features, so disable them: conflicting with
	;; the eslint settings.
	js2-strict-trailing-comma-warning nil
	js2-strict-missing-semi-warning nil
	;; maximum fontification
	js2-highlight-level 3
	js2-highlight-external-variables t
	js2-idle-timer-delay 0.1)
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'add-node-modules-path))

;; NPM support
(use-package npm-mode)

;; Add node_modules path
(use-package add-node-modules-path
  :config
  (add-hook 'typescript-mode-hook 'add-node-modules-path)
  (add-hook 'javascript-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'add-node-modules-path))

;; GraphQL mode
(use-package graphql-mode)

(use-package prettier-js)

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

(provide 'init-javascript)

;;; init-javascript.el ends here
