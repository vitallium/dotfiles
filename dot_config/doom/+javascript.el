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

;; Fix stylelint >= v14:
;; See https://github.com/flycheck/flycheck/pull/1944
(flycheck-define-checker general-stylelint
  "A checker for CSS and related languages using Stylelint"
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-general-stylelintrc)
            "--stdin-filename" (eval (or (buffer-file-name) "style.scss")))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (scss-mode))
(flycheck-def-config-file-var flycheck-general-stylelintrc
    (general-stylelint) nil)
(add-to-list 'flycheck-checkers 'general-stylelint)
