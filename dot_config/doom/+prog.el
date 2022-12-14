;;; $DOOMDIR/+prog.el -*- lexical-binding: t; -*-

;; TODO: Split this file into private modules per lang

;; JavaScript
(use-package jest
  :after (js2-mode rjsx-mode)
  :config
  (setq jest-executable "yarn jest")
  (pushnew! evil-collection-mode-list 'jest-mode)
  (pushnew! evil-normal-state-modes 'jest-mode)
  (set-popup-rule! "^\\*jest\\*"
    :size 0.5
    :ttl nil :select t))

(add-hook! (js2-mode rjsx-mode) #'jest-minor-mode)

(after! web-mode
  (web-mode-toggle-current-element-highlight))

;; LSP
(after! lsp-mode
  (map! :leader
        :desc "Diagnostics" "c-" #'lsp-ui-flycheck-list
        :desc "Imenu" "c," #'lsp-ui-imenu)
  ;; Core
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        ;; Smoother LSP features response in cost of performance (Most servers I use have good performance)
        lsp-idle-delay 0.2
        lsp-use-plists nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        lsp-enable-server-download nil
        lsp-enable-suggest-server-download nil
        lsp-eslint-package-manager "yarn")
  (remove-hook 'lsp-mode-hook #'+lsp-init-flycheck-or-flymake-h))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil))

;; (setq flycheck-javascript-eslint-executable "eslint_d")

(defun +js/fix-checker ()
  "Fix LSP overwritten checkers."
  (interactive)
  (when (-contains? '(js2-mode rjsx-mode) major-mode)
    (flycheck-select-checker 'javascript-eslint)))

(add-hook 'lsp-mode-hook #'+js/fix-checker)

;; Ruby
(defvar ruby-disabled-checkers '(ruby-reek lsp ruby-rubylint) "Checkers to automatically disable on ruby files.")
(add-hook! 'ruby-mode-hook (setq-local flycheck-disabled-checkers ruby-disabled-checkers))

;; Rspec Stuff
(after! rspec-mode
  (set-popup-rule! "^\\*\\(rspec-\\)?compilation" :size 0.5 :ttl nil :select t)
  (map! :leader :desc "Rspec" "t" #'rspec-mode-keymap)
  (map! :leader :desc "Run Last Failed" "tl" #'rspec-run-last-failed))

(after! ruby-mode
  (map! :mode ruby-mode :leader :desc "Go to Test" "a" 'goto-test)
  (map! :mode ruby-mode :leader :desc "Go to Test and split" "A" 'goto-test-and-vsplit))

(add-hook! 'ruby-mode-hook (setq-local flycheck-checker 'ruby-rubocop))
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))

;; Vue
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(add-hook!
  (js2-mode
   rjsx-mode
   typescript-mode
   web-mode)
  #'apheleia-mode)

;; Activate bug-reference-prog-mode
(after! prog-mode
  (bug-reference-prog-mode))

(after! lsp-solargraph
  (add-to-list 'lsp-solargraph-library-directories "~/.asdf/installs/ruby"))
