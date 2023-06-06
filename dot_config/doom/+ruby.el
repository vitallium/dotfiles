;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(after! rspec-mode
  (set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :side 'bottom :quit 'current))

(when (modulep! :tools lsp)
  (after! lsp-solargraph
    ;; Add asdf installations for Ruby
    (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby")
    ;; Ignore asdf for LSP file watcher
    (add-to-list 'lsp-file-watch-ignored-directories "~/.asdf")))

(set-docsets! 'ruby-mode "Ruby_3" "Ruby_on_Rails_7")
