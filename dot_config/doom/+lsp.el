;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq
   ;; Do not ask to download and install LSP
   lsp-enable-suggest-server-download nil
   lsp-semantic-tokens-enable t
   lsp-enable-folding t
   lsp-signature-render-documentation nil
   lsp-signature-function 'lsp-signature-posframe
   ;; Improve LSP performance
   lsp-idle-delay 1
   lsp-log-io nil
   read-process-output-max (* 1024 1024 4)
   ;; Add mise installations for Ruby
   ;; (add-to-list 'lsp-solargraph-library-directories "~/.local/share/mise/installs/ruby")
   ;; Solargraph settings
   lsp-solargraph-use-bundler t)
  ;; Add "tmp" and ".devbox" directories to ignored list of directories.
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].devbox")
  ;; See https://github.com/emacs-lsp/lsp-mode/issues/3577
  (delete 'lsp-terraform lsp-client-packages)
  (delete 'lsp-volar lsp-client-packages)

  ;; Rust
  (when (modulep! :lang rust +lsp)
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
  ;; Configure lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-use-webkit t
        lsp-signature-render-documentation nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil))
