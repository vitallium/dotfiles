;;; $DOOMDIR/+prog.el -*- lexical-binding: t; -*-

(after! company
  ;; Only complete when asked
  (setq company-idle-delay nil))

;; JavaScript
(use-package jest
  :after (js2-mode)
  :config
  (setq jest-executable "yarn jest")
  (pushnew! evil-collection-mode-list 'jest-mode)
  (pushnew! evil-normal-state-modes 'jest-mode)
  (set-popup-rule! "^\\*jest\\*"
    :side 'bottom
    :size 0.25
    :quit 'current :ttl nil))

(add-hook! (js2-mode rjsx-mode) #'jest-minor-mode)

(after! web-mode
  (web-mode-toggle-current-element-highlight))

;; LSP
(after! lsp-mode
  (map! :leader
        :desc "Diagnostics" "c-" #'lsp-ui-flycheck-list
        :desc "Imenu" "c," #'lsp-ui-imenu)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-imenu--custom-mode-line-format ""
        +lsp-company-backends '(company-capf company-yasnippet)
          ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        lsp-enable-server-download nil
        lsp-enable-suggest-server-download nil
        lsp-eslint-package-manager "yarn")
  (remove-hook 'lsp-mode-hook #'+lsp-init-flycheck-or-flymake-h))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K

;; (setq flycheck-javascript-eslint-executable "eslint_d")

(defun +js/fix-checker ()
  "Fix LSP overwritten checkers."
  (interactive)
  (when (-contains? '(js2-mode rjsx-mode) major-mode)
    (flycheck-select-checker 'javascript-eslint)))

(add-hook 'lsp-mode-hook #'+js/fix-checker)

;; Ruby
(setq flycheck-disabled-checkers '(ruby-reek))

(add-hook! 'ruby-mode-hook (setq-local flycheck-checker 'ruby-rubocop))
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))

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
