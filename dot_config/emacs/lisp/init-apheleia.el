;;; init-apheleia.el --- Formatter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package apheleia
  :diminish
  :defer t
  :hook ((prog-mode . apheleia-mode))
  :commands (apheleia-mode apheleia-global-mode apheleia-format-buffer))

;; TODO: Check major modes where apheleia is not required.

(provide 'init-apheleia)
;;; init-apheleia.el ends here
