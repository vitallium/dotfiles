;; LSP
(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 10))

;; LSP UI
;; Fancy sideline, popup documentation, VScode-like peek UI, etc.
(use-package lsp-ui
  :config
  (setq lsp-enable-snippet nil))

;; Company (text completion framework)
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
	company-idle-delay 0
	company-minimum-prefix-length 2
	company-selection-wrap-around t))

;; Ivy (completion for minibuffers)
(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper)

;; Integrate Ivy into LSP
(use-package lsp-ivy
  :after lsp-mode)

(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(provide 'init-lsp)

;;; init-lsp.el ends here
