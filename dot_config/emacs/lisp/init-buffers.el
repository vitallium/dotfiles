;;; init-buffers.el --- Buffer management configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :elpaca nil
  :config
  (setq recentf-save-file (expand-file-name (concat user-emacs-cache-directory "recentf")))
  :init
  (recentf-mode))

(use-package minibuffer
  :elpaca nil
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t))

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
