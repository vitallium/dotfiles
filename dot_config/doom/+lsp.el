;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq
   ;; Do not ask to download and install LSP
   lsp-enable-suggest-server-download nil
   ;; lsp-signature-function 'lsp-signature-posframe
   ;; Improve LSP performance
   read-process-output-max (* 1024 1024 4))

  ;; Add "tmp" and ".devbox" directories to ignored list of directories.
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].devbox")

  (setq lsp-json-schemas
        `[
          ;; From https://github.com/b0o/SchemaStore.nvim/blob/main/lua/schemastore/catalog.lua#L2796
          (:fileMatch [".prettierrc" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml"] :url "https://json.schemastore.org/prettierrc.json")
          ]))

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-use-webkit t
        lsp-signature-render-documentation nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil))
