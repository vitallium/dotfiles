;;; init-magit.el --- Magit integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer t
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-define-global-key-bindings t)
  (transient-suffix-put 'magit-dispatch "k" :key "x")
  :bind
  ("C-c g d" . magit-diff-range)
  ("C-x g" . magit-status))

;; diff-hl - highlight changes/diffs
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode))

(provide 'init-magit)
;;; init-magit.el ends here
