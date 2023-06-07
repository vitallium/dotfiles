;;; $DOOMDIR/+javascript.el -*- lexical-binding: t; -*-

(use-package! jest-test-mode
  :hook ((typescript-mode js2-mode typescript-tsx-mode) . jest-test-mode)
  :config
  (when (modulep! :editor evil)
    (add-hook 'jest-test-mode-hook #'evil-normalize-keymaps))
  (set-popup-rule! "^\\*\\(jest-\\)?compilation" :size 0.3 :ttl nil :select t)
  ;; Add keybindings for jest-test-mode
  (map! :localleader
        :map jest-test-mode-map
        :prefix "t"
        "r" #'jest-test-mode-rerun
        "a" #'jest-test-mode-run-all
        "s" #'jest-test-run-at-point))

(pushnew! auto-mode-alist
          '("\\manifest.webapp\\'" . json-mode)
          '("\\.eslintrc\\'" . json-mode)
          '("\\.prettierrc\\'" . json-mode)
          '("\\.babelrc\\'" . json-mode))

(set-docsets! 'js2-mode "JavaScript")

(after! web-mode
  (web-mode-toggle-current-element-highlight))
