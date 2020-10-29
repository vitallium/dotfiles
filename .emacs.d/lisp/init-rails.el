;; Ruby mode
(use-package ruby-mode
  :config
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (setq ruby-insert-encoding-magic-comment nil))

;; Rubocop
(use-package rubocop
  :config
  (setq rubocop-check-command "rubocop --format emacs")
  (setq rubocop-autocorrect-command "rubocop -a --format emacs"))

;; Use 'bundle exec' for running Rubocop
(add-hook 'rubocop-mode-hook
	  (lambda ()
	    (make-variable-buffer-local 'flycheck-command-wrapper-function)
	    (setq flycheck-command-wrapper-function
		  (lambda (command)
		    (append '("bundle" "exec") command)))))

;; RSpec mode
(use-package rspec-mode
  :after ruby-mode)

;; Rails + Projectile
(use-package projectile-rails
  :after projectile)
(use-package inflections
  :after projectile-rails)

(provide 'init-rails)

;;; init-rails.el ends here
