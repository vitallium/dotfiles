;;; $DOOMDIR/+vterm.el -*- lexical-binding: t; -*-

(setq shell-file-name (executable-find "bash"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Always compile vterm-module and do not ask anything
(set-popup-rule! "\\*Installl vterm\\*.*" :actions (cons #'display-buffer-no-window nil))
(setq vterm-always-compile-module t)
(setq-default vterm-shell (executable-find "fish"))

(after! vterm
  (when (modulep! :editor evil)
    ;; Start vterm in insert mode always
    (evil-set-initial-state 'vterm-mode 'insert))

  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files))))


(use-package! multi-vterm
  :after vterm)
