;;; init-lang-ruby.el --- Ruby configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode "\\.erb\\'")

(use-package ruby-mode
  :elpaca nil
  :diminish
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  :hook
  (ruby-mode . superword-mode))

(use-package projectile-rails
  :after projectile
  :defer t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package rspec-mode
  :after ruby-mode
  :config
  (setq rspec-use-spring-when-possible nil))

(use-package yard-mode
  :hook
  (ruby-mode . yard-mode))

(use-package ruby-tools
  :diminish ruby-tools-mode
  :hook
  (ruby-mode-hook . ruby-tools-mode))

(provide 'init-lang-ruby)
;;; init-lang-ruby.el ends here
