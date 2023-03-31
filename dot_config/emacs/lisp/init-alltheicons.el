;;; init-alltheicons.el --- All the icons configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons
  :elpaca (:host github :repo "domtronn/all-the-icons.el")
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :elpaca (:host github :repo "iyefrat/all-the-icons-completion")
  :after all-the-icons
  :config
  (all-the-icons-completion-mode))

(provide 'init-alltheicons)
;;; init-alltheicons.el ends here