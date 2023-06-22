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

(after! haml-mode
  (after! flycheck
    (flycheck-define-checker haml-lint
        "A haml syntax checker using the haml-lint tool."
        :command ("bundle"
                  "exec"
                  "haml-lint"
                  source-inplace)
        :working-directory flycheck-ruby--find-project-root
        :error-patterns
        ((info line-start (file-name) ":" line " [C] " (message) line-end)
        (warning line-start (file-name) ":" line " [W] " (message) line-end)
        (error line-start (file-name) ":" line " [" (or "E" "F") "] " (message) line-end))
        :modes (haml-mode))
      (add-to-list 'flycheck-checkers 'haml-lint)
      (flycheck-add-next-checker 'haml '(warning . haml-lint))

      (add-to-list 'compilation-error-regexp-alist-alist
                   '(haml-lint
                     "^\\([^:]+\\):\\([0-9]+\\) \\[\\(W\\|E\\)\\] "
                     1 2))
      (add-to-list 'compilation-error-regexp-alist 'haml-lint)))
