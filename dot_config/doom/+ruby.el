;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(set-docsets! 'ruby-base-mode "Ruby_3" "Ruby_on_Rails_7")

(after! lsp-solargraph
  ;; Add mise installations for Ruby
  (add-to-list 'lsp-solargraph-library-directories "~/.local/share/mise/installs/ruby")
  (setq lsp-solargraph-use-bundler t))

;; Treat underscores as part of words
(add-hook! 'ruby-base-mode-hook
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  ;; Disable vue-semantic-server as it takes over Ruby buffers for some reason
  (when (modulep! :tools lsp) (setq-local lsp-disabled-clients '(vue-semantic-server))))
