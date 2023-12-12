;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp)
  (after! lsp-solargraph
    ;; Add asdf installations for Ruby
    (add-to-list 'lsp-solargraph-library-directories "~/.local/share/rtx/installs/ruby")
    (setq lsp-solargraph-use-bundler t)))

(set-docsets! 'ruby-mode "Ruby_3" "Ruby_on_Rails_7")

;; Treat underscores as part of words
(add-hook! 'ruby-mode-hook (modify-syntax-entry ?_ "w" ruby-mode-syntax-table))

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

(use-package! ruby-refactor
  :hook ((ruby-base-mode . ruby-refactor-mode-launch)))

(transient-define-prefix vitallium/ruby-refactor-transient ()
  "My custom Transient menu for Ruby refactoring."
  [["Refactor"
    ("e" "Extract Region to Method" ruby-refactor-extract-to-method)
    ("v" "Extract Local Variable" ruby-refactor-extract-local-variable)
    ("l" "Extract to let" ruby-refactor-extract-to-let)
    ("c" "Extract Constant" ruby-refactor-extract-constant)
    ;; ("r" "Rename Local Variable or Method (LSP)" eglot-rename)
    ("{" "Toggle block style" ruby-toggle-block)
    ("'" "Toggle string quotes" ruby-toggle-string-quotes)
    ]
   ["Actions"
    ("d" "Documentation Buffer" eldoc-doc-buffer)
    ("q" "Quit" transient-quit-one)
    ("C" "Run a REPL" inf-ruby-console-auto)
    ("TAB" "Switch to REPL" ruby-switch-to-inf)]])
