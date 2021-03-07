;;; private/evil/config.el -*- lexical-binding: t; -*-

(setq
 ;; Switch to the new window after splitting
 evil-split-window-below t
 evil-vsplit-window-right t
 ;; By default while in insert all changes are one big blob. Be more granular.
 evil-want-fine-undo t)

(after! evil-escape (evil-escape-mode -1))
(after! evil (setq evil-ex-substitute-global t))

;; (setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))
