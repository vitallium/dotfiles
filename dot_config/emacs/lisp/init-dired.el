;;; init-dired.el --- Dired configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dirvish
  :defer t
  :config
  (setq insert-directory-program "gls"
		dired-listing-switches
		"-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :init
  (dirvish-override-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
