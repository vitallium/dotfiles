(setq-hook! 'ruby-mode-hook +format-with :none)

;;(add-hook! rspec-compilation-mode #'inf-ruby-switch-from-compilation)

(add-hook! 'ruby-mode-hook (setq-local flycheck-checker 'ruby-rubocop))
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))
