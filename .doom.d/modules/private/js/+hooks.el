(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)

;; Prettier
(defun maybe-use-prettier()
  "Enable prettier-js-mode if an rc file is located."
  (if (locate-dominating-file default-directory ".prettierrc")
      (prettier-mode +1)))

(add-hook! js2-mode 'maybe-use-prettier)
(add-hook! rjsx-mode-hook 'maybe-use-prettier)

(setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)

;; Jest
(after! jest
  (setq jest-executable "yarn jest"))
(add-hook! js2-mode #'jest-minor-mode)
