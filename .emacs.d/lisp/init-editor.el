;; Indent guides
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character))

;; Prettify symbols
(global-prettify-symbols-mode 1)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Modeline
(use-package doom-modeline
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  (use-package all-the-icons)
  (doom-modeline-mode 1))

;; Smart paren pairs
(use-package smartparens
  :diminish
  :config
  (smartparens-global-mode 1)
  (show-paren-mode t))

;; EditorConfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(provide 'init-editor)

;;; init-editor.el ends here
