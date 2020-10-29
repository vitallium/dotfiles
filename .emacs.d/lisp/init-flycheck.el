;; Flycheck
(use-package flycheck
  :init
  (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-idle-change-delay 2)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-highlighting-mode 'lines)
  :config
  (global-flycheck-mode))

(provide 'init-flycheck)

;; init-flycheck.el ends here
