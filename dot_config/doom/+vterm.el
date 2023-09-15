;;; $DOOMDIR/+vterm.el -*- lexical-binding: t; -*-

(setq shell-file-name (executable-find "bash"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Always compile vterm-module and do not ask anything
(set-popup-rule! "\\*Installl vterm\\*.*" :actions (cons #'display-buffer-no-window nil))
(setq vterm-always-compile-module t)
(setq-default vterm-shell (executable-find "fish"))

(after! vterm
  ;; Start vterm in insert mode always
  (evil-set-initial-state 'vterm-mode 'insert)
  ;; Aliases
  (setf (alist-get "woman" vterm-eval-cmds nil nil #'equal)
        '((lambda (topic)
            (woman topic))))
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path))))
  (setf (alist-get "dired" vterm-eval-cmds nil nil #'equal)
        '((lambda (dir)
            (dired dir)))))

(use-package! multi-vterm
  :after vterm)
