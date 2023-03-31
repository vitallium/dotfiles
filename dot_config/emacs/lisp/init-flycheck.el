;;; init-flycheck.el --- Error checker configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :hook
  (flycheck-mode . flycheck-color-mode-line-mode)
  :after flycheck)

(provide 'init-flycheck)
;;; init-flycheck.el ends here