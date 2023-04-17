;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-ui
    :config
    (setq lsp-ui-doc-delay 2
          lsp-ui-doc-max-width 80)
    (setq lsp-signature-function 'lsp-signature-posframe))
