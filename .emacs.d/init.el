;;; init.el -*- lexical-binding: t; -*-

;; Add out path to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Because I use emacs on macOS too
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Basic stuff
(require 'init-use-package)
(require 'init-defaults)
(require 'init-editor)
(require 'init-evil)
(require 'init-maps)

;;
(require 'init-projectile)
(require 'init-flycheck)

;; Programming stuff
(require 'init-lsp)
(require 'init-vc)

;; Languages
(require 'init-rails)

(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(provide 'init)

;;; init.el ends here

