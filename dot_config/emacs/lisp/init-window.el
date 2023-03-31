;;; init-window.el --- Windows related configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package window
  :elpaca nil
  :config
  (setq switch-to-buffer-obey-display-actions t
    display-buffer-alist
    '(("\\*\\(less-css-compilation\\|compilation\\)\\*"
       (display-buffer-no-window))
      ("\\*\\e?shell\\*"
       (display-buffer-in-side-window)
       (window-height . 0.50)
       (side . bottom)
       (slot . 1))
      ((or (major-mode . flymake-project-diagnostics-mode)
           (major-mode . flymake-diagnostics-buffer-mode))
       (display-buffer-in-side-window)
       (window-height . 0.30))
      ("\\*\\(ansi-term\\|terminal\\)\\*"
       (display-buffer-in-side-window)
       (window-height . 0.40)
       (side . bottom)
       (slot . -1))
      ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
       (display-buffer-in-side-window)
       (window-height . 0.25)
       (side . bottom)
       (slot . 0))
      ("\\*eldoc\\*"
       (display-buffer-in-side-window)
       (window-height . 0.25)
       (side . bottom)
       (slot . 1))
      ("\\*\\([Hh]elp\\|Apropos\\)\\*"
       (display-buffer-in-side-window)
       (window-height . 0.30)
       (side . bottom)
       (slot . 0)
       (window-parameters . ((mode-line-format . none)))))))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("M-o" . ace-window))

(provide 'init-window)
;;; init-window.el ends here
