;;; $DOOMDIR/+vterm.el -*- lexical-binding: t; -*-

;; Always compile vterm-module and do not ask anything
(set-popup-rule! "\\*Installl vterm\\*.*" :actions (cons #'display-buffer-no-window nil))
(setq vterm-always-compile-module t)

(after! vterm
  ;; Start vterm in insert mode always
  (evil-set-initial-state 'vterm-mode 'insert))
