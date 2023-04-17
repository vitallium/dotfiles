;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))
(add-hook! 'ruby-mode-hook (setq-local flycheck-checker 'ruby-rubocop))

;; (if (modulep! :tools lsp)
;;   (after! lsp-mode
;;     (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby")))
