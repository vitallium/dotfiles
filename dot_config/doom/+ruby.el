;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp)
  (after! lsp-solargraph
    ;; Add asdf installations for Ruby
    (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby")
    ;; Ignore asdf for LSP file watcher
    (add-to-list 'lsp-file-watch-ignored-directories "~/.asdf")))

(set-docsets! 'ruby-mode "Ruby_3" "Ruby_on_Rails_7")

;; Treat underscores as part of words
(add-hook! ruby-mode #'(lambda() (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)))
