;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(setq read-process-output-max (* 1024 1024 4))

(after! lsp-mode
  ;; Core
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        lsp-idle-delay 0.05) ;; Smoother LSP features response in cost of performance (Most servers I use have good performance))
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))
  (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin)))

  (when (modulep! :completion company)
    (setq +lsp-company-backends '(company-capf company-files company-yasnippet :separate company-tabnine)))

  ;; Rust
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t))

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil))
