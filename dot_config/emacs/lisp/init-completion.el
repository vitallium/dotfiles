;;; init-completion.el --- General completion-related configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :config
  (setq tab-always-indent 'complete
        completion-cycle-threshold 2)
  (global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :config
  (setq savehist-file (expand-file-name (concat user-emacs-cache-directory "history")))
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package pulsar
  :custom
  (pulsar-iterations 15)
  (pulsar-face 'pulsar-red)
  :config
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'imenu-after-jump-hook #'pulsar-pulse-line)
  (pulsar-global-mode))

(provide 'init-completion)
;;; init-completion.el ends here
