;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp)
  (after! lsp-solargraph
    ;; Add asdf installations for Ruby
    (add-to-list 'lsp-solargraph-library-directories "~/.local/share/mise/installs/ruby")))

(set-docsets! 'ruby-base-mode "Ruby_3" "Ruby_on_Rails_7")

;; Treat underscores as part of words
(add-hook! 'ruby-base-mode-hook
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  (when (modulep! :tools lsp) (setq-local lsp-disabled-clients '(vue-semantic-server))))

(use-package! ruby-refactor
  :hook ((ruby-base-mode . ruby-refactor-mode-launch)))
