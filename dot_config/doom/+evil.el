;;; $DOOMDIR/+evil.el -*- lexical-binding: t; -*-

(after! evil
  (setq evil-ex-substitute-global t    ;; I like my s/../.. to be global by default
        evil-move-cursor-back nil      ;; Do not move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil  ;; Do not put overwritten text in the kill ring
        evil-vsplit-window-right t
        evil-split-window-below t))
