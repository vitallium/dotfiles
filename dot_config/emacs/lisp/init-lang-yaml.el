;;; init-lang-yaml.el --- YAML configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-mode . (lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
  :after tree-sitter)

(provide 'init-lang-yaml)
;;; init-lang-yaml.el ends here