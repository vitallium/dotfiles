;;; $DOOMDIR/+vterm.el -*- lexical-binding: t; -*-

(after! vterm
  ;; Start vterm in insert mode always
  (evil-set-initial-state 'vterm-mode 'insert))
