;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))
(add-hook! 'ruby-mode-hook (setq-local flycheck-checker 'ruby-rubocop))

(if (modulep! :tools lsp)
  ;; Add asdf installations
  (after! lsp-solargraph
    (add-to-list 'lsp-solargraph-library-directories (expand-file-name "~/.asdf/installs/ruby/"))
    ;; Ignore asdf for LSP file watcher
    (add-to-list 'lsp-file-watch-ignored-directories (expand-file-name "~/.asdf"))))

(add-hook 'ruby-mode-hook 'robe-mode)
