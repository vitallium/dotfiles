;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

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

(defun +js/fix-checker ()
  "Fix LSP overwritten checkers."
  (interactive)
  (when (-contains? '(js2-mode rjsx-mode) major-mode)
    (flycheck-select-checker 'javascript-eslint)))

(add-hook 'lsp-mode-hook #'+js/fix-checker)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(add-hook!
  (js2-mode
   rjsx-mode
   typescript-mode
   web-mode)
  #'apheleia-mode)
