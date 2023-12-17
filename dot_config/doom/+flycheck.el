;;; $DOOMDIR/+flycheck.el -*- lexical-binding: t; -*-

;; Configure flycheck
(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-javascript-eslint-executable "eslint_d"
        flycheck-stylelintrc ".stylelintrc.json"
        flycheck-global-modes '(not org-mode)))

;; Sometimes the posframe is not going away so we have
;; to install a monitor for hiding it.
;; See https://github.com/doomemacs/doomemacs/issues/6416
(when (modulep! :checkers syntax +childframe)
  (defun flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))

  (defun fix-flycheck-posframe-not-hide-immediately ()
    (cond (flycheck-posframe-mode
           (add-hook 'post-command-hook 'flycheck-posframe-monitor-post-command nil t))
          ((not flycheck-posframe-mode)
           (remove-hook 'post-command-hook 'flycheck-posframe-monitor-post-command t))))
  (add-hook! flycheck-posframe-mode #'fix-flycheck-posframe-not-hide-immediately))
