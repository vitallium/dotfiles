;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(after! rspec-mode
  (add-hook! rspec-compilation-mode #'inf-ruby-switch-from-compilation)
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.25 :side 'right :quit 'current))

(after! lsp-solargraph
  ;; Add asdf installations for Ruby
  (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby")
  ;; Ignore asdf for LSP file watcher
  (add-to-list 'lsp-file-watch-ignored-directories "~/.asdf"))

(after! flycheck
  (flycheck-add-next-checker 'ruby-reek 'ruby-rubocop)
  (setq flycheck-ruby-rubocop-executable "exec")
  (setq flycheck-ruby-rubocop-prefix ["bundle"])
  (setq flycheck-ruby-rubocop-command
        (append flycheck-ruby-rubocop-prefix (list flycheck-ruby-rubocop-executable "rubocop"))))
