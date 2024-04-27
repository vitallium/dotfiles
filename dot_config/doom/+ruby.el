;;; $DOOMDIR/+ruby.el -*- lexical-binding: t; -*-

(set-docsets! 'ruby-base-mode "Ruby_3" "Ruby_on_Rails_7")

(after! lsp-solargraph
  ;; Add mise installations for Ruby
  (add-to-list 'lsp-solargraph-library-directories "~/.local/share/mise/installs/ruby"))

;; Treat underscores as part of words
(add-hook! 'ruby-base-mode-hook
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  ;; Disable vue-semantic-server as it takes over Ruby buffers for some reason
  (when (modulep! :tools lsp) (setq-local lsp-disabled-clients '(vue-semantic-server)))
  (set-lookup-handlers! 'ruby-base-mode
    :definition '(projectile-rails-goto-file-at-point robe-jump)
    :documentation #'robe-doc))

(use-package! ruby-refactor
  :hook
  (ruby-base-mode . ruby-refactor-mode)
  :config
  (map! :mode ruby-mode :localleader "v" :desc "Extract Local Variable" 'ruby-refactor-extract-local-variable)
  (map! :mode ruby-mode :localleader "V" :desc "Extract Constant" 'ruby-refactor-extract-constant)
  (map! :mode ruby-mode :localleader "m" :desc "Extract to Method" 'ruby-refactor-extract-to-method))

(after! rotate-text
  (add-to-list 'rotate-text-words '("valid" "invalid"))
  (add-to-list 'rotate-text-words '("context" "describe"))
  (add-to-list 'rotate-text-symbols '("be_valid" "be_invalid"))
  (add-to-list 'rotate-text-symbols '("valid?" "invalid?"))
  (add-to-list 'rotate-text-symbols '("present?" "blank?" "nil?"))
  (add-to-list 'rotate-text-symbols '("belongs_to" "has_many" "has_one"))
  (add-to-list 'rotate-text-symbols '("if" "unless"))
  (add-to-list 'rotate-text-symbols '("greater_than" "greater_than_or_equal_to" "equal_to" "less_than" "less_than_or_equal_to" "other_than" "odd" "even"))
  (add-to-list 'rotate-text-symbols '("to" "not_to")))
