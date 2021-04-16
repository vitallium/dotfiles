(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-enable-symbol-highlighting nil
        lsp-idle-delay 0.5))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil))

(after! flycheck
  (setq flycheck-display-errors-delay 0.1))
