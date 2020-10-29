;; Evil mode
(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Add Evil mode in other parts of Emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; TODO: desc
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

;; General
(use-package general
  :config
  (general-evil-setup t))

(provide 'init-evil)
