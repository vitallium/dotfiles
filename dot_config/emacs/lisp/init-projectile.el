;;; init-projectile.el --- Project management configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rg)

(use-package projectile
  :diminish
  :after rg
  :init
  (setq projectile-enable-caching t
        projectile-sort-order 'recently-active
        projectile-indexing-method 'alien
		projectile-cache-file (expand-file-name (concat user-emacs-cache-directory "projectile.cache"))
		projectile-known-projects-file (expand-file-name (concat user-emacs-cache-directory "projecile-boomarks.eld")))
  :custom
  ;; quick fix for bbatsov/projectile#1777
  (projectile-globally-ignored-directories . nil)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'init-projectile)
;;; init-projectile.el ends here
