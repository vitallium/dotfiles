(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.5))

(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions nil))

(after! flycheck
  (setq flycheck-display-errors-delay 0.1))
