;;; $DOOMDIR/+json.el -*- lexical-binding: t; -*-

(pushnew! auto-mode-alist
          '("\\manifest.webapp\\'" . json-mode)
          '("\\.eslintrc\\'" . json-mode)
          '("\\.prettierrc\\'" . json-mode)
          '("\\.babelrc\\'" . json-mode))
