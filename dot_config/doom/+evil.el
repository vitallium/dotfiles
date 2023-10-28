;;; $DOOMDIR/+evil.el -*- lexical-binding: t; -*-

(after! evil
  (setq
   ;; I like my s/../.. to be global by default
   evil-ex-substitute-global t
   ;; Do not move the block cursor when toggling insert mode
   evil-move-cursor-back nil
   ;; Do not put overwritten text in the kill ring
   evil-kill-on-visual-paste nil
   evil-vsplit-window-right t
   evil-split-window-below t
   ;; By default while in insert all changes are one big blob. Be more granular
   evil-want-fine-undo t))

;; Re-enable `smerge-mode` that was disabled in
;; https://github.com/doomemacs/doomemacs/commit/fe3f8866d81f32ef4edb2cd5be7214ad86b65447
;; due to the upstream bug
;; https://github.com/emacs-evil/evil-collection/pull/705 which is now resolved
(after! evil-collection
  (progn
    (add-to-list 'evil-collection-mode-list 'smerge-mode)
    (+evil-collection-init 'smerge-mode)))
