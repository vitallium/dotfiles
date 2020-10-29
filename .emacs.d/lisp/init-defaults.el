;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Use PATH from shell
(use-package exec-path-from-shell)

;; Disable backup files
(setq confirm-kill-processes nil
      create-lockfiles nil
      make-backup-files nil)

(require 'xdg)
;; Keep Emacs directory clean
(use-package no-littering
  :init
  (setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
      (expand-file-name "emacs/" (xdg-data-home)))
  (setq no-littering-tmp-directory
      (expand-file-name "emacs/" (xdg-cache-home))))

;; Change YES-NO to Y-N
(fset 'yes-or-no-p 'y-or-n-p)

;; Use UTF-8 everywhere
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Automatically refresh buffers if changed outside of Emacs
(global-auto-revert-mode +1)
(setq auto-revert-interval 2
      auto-revert-check-vs-info t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Disable splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Do not show some common modes in the modeline
(use-package diminish)

;; Don't prompt to confirm theme safety
(setq custom-safe-themes t)

;; Theme
(use-package doom-themes
  :config
  (doom-themes-org-config))

(load-theme 'doom-flatwhite)

;; Font
(set-face-attribute 'default nil
		    :family "Iosevka SS14"
		    :height 120)

;; Recentf
(use-package recentf
  :straight nil
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-tmp-directory)
  (recentf-mode 1))

;;
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame
	aw-background t))
(use-package winum
  :config
  (winum-mode +1))

(provide 'init-defaults)

;;; init-defaults.el ends here
