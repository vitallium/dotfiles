;; Projectile
(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-git-submodule-command nil)
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel)

;; Counsel integration for Projectile
(use-package counsel-projectile
  :after projectile counsel
  :config
  (counsel-projectile-mode))

;; Company mode (autocomplete)
(use-package company
  :init
    (global-company-mode)
  :config
    (setq company-idle-delay 0.2))

(provide 'init-projectile)

;;; init-projectile.el ends here
