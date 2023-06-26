;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  ;; Do not ask to download and install LSP
  (setq lsp-enable-suggest-server-download nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable t
        ;; Improve LSP performance
        lsp-idle-delay 1
        lsp-log-io nil
        read-process-output-max (* 1024 1024 4))
  (add-hook! 'lsp-mode-hook (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))

  (when (modulep! :completion company)
    (setq +lsp-company-backends '(company-capf company-files company-yasnippet :separate company-tabnine)))

  ;; Rust
  (when (modulep! :lang rust)
    (setq lsp-rust-analyzer-server-display-inlay-hints t
          lsp-rust-analyzer-display-parameter-hints t
          lsp-rust-analyzer-display-chaining-hints t))

  (setq lsp-json-schemas
        `[
          ;; From https://github.com/b0o/SchemaStore.nvim/blob/main/lua/schemastore/catalog.lua#L2796
          (:fileMatch [".prettierrc" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml"] :url "https://json.schemastore.org/prettierrc.json")
          ]))

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))

(after! lsp-ui
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 20
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-header t
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-use-webkit t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-diagnostics t
        lsp-ui-sideline-hover nil
        lsp-ui-sideline-update-mode #'line
        lsp-ui-peek-enable t
        lsp-ui-imenu-kind-position 'left))
;; lsp-ui-doc is redundant with and more invasive than `+lookup/documentation'
;; lsp-ui-doc-enable nil))
