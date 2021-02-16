(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-diagnostics-provider 'flycheck
        lsp-lens-enable t))

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover nil
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t))
