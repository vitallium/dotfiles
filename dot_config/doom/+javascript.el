;;; $DOOMDIR/+javascript.el -*- lexical-binding: t; -*-

(use-package! jest-test-mode
  :when (modulep! :lang javascript +jest)
  :hook ((js-mode typescript-mode) . jest-test-mode)
  :config
  (when (modulep! :editor evil)
    (add-hook 'jest-test-mode-hook #'evil-normalize-keymaps))
  (set-popup-rule! "^\\*\\(jest-\\)?compilation" :size 0.3 :ttl nil :select t)
  (map! :localleader
        :map jest-test-mode-map
        :prefix "t"
        "r" #'jest-test-rerun-test
        "t" #'jest-test-run
        "a" #'jest-test-run-all-tests
        "s" #'jest-test-run-at-point))

(set-docsets! 'js-mode "JavaScript")

(after! web-mode
  (web-mode-dom-errors-show)
  (web-mode-toggle-current-element-highlight))

