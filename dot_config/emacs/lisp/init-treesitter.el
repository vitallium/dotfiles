;;; init-treesitter.el --- Tree sitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :elpaca nil
  :config
  (setq treesit-language-source-alist
  '((css "https://github.com/tree-sitter/tree-sitter-css")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(ruby-mode . ruby-ts-mode) major-mode-remap-alist)
  (push '(rust-mode . rust-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  (push '(yaml-mode . yaml-ts-mode) major-mode-remap-alist)
  (push '(dockerfile-mode . dockerfile-ts-mode) major-mode-remap-alist))

(provide 'init-treesitter)
;;; init-treesitter.el ends here