;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp)
  (after! lsp-solargraph
    ;; Add asdf installations for Ruby
    (add-to-list 'lsp-solargraph-library-directories "~/.local/share/rtx/installs/ruby")))

(set-docsets! 'ruby-base-mode "Ruby_3" "Ruby_on_Rails_7")

;; Treat underscores as part of words
(add-hook! 'ruby-base-mode-hook
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  (when (modulep! :tools lsp) (setq-local lsp-disabled-clients '(vue-semantic-server))))

(after! haml-mode
  (after! flycheck
    (flycheck-define-checker haml-lint
      "A HAML syntax checker using the haml-lint tool."
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

(use-package! ruby-refactor
  :hook ((ruby-base-mode . ruby-refactor-mode-launch)))
