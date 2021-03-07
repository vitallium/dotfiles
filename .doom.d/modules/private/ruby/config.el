(setq flycheck-disabled-checkers '(ruby-reek))

(map! :mode ruby-mode
      (:leader
       (:prefix "p"
        :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test)))

(after! lsp-solargraph
  (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby"))

(load! "+hooks")
