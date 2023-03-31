;;; init-eglot.el --- Language server configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((python-ts-mode . eglot-ensure)
   (ruby-ts-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (java-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (yaml-mode . eglot-ensure))
  :bind (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e r" . eglot-rename)
        ("C-c e f" . eglot-format)
        ("C-c x r" . xref-find-references)
        ("C-c x d" . xref-find-definitions)
        ("C-c f n" . flymake-goto-next-error)
        ("C-c f p" . flymake-goto-prev-error)
        ("C-c f d" . flymake-show-project-diagnostics))
  :custom
  (eglot-menu-string "LSP")
  (eglot-report-progress nil)
  (eglot-connect-timeout 10)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 2)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-workspace-configuration '(:solargraph (:formatting t))))

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  (dolist (cmd '(corfu-insert avy-jump))
    (eldoc-add-command cmd)))

(use-package consult-eglot
  :after eglot
  :commands (consult-eglot-symbols))

(provide 'init-eglot)
;;; init-eglot.el ends here