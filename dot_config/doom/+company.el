;;; $DOOMDIR/+company.el -*- lexical-binding: t; -*-

(use-package! tabnine
  :when (modulep! :completion company +tabnine)
  :after company
  :hook ((prog-mode . tabnine-mode)
         (markdown-mode . tabnine-mode)
         (kill-emacs . tabnine-kill-process))
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  :bind
  (:map  tabnine-completion-map
         ("<tab>" . tabnine-accept-completion)
         ("TAB" . tabnine-accept-completion)
         ("M-f" . tabnine-accept-completion-by-word)
         ("M-<return>" . tabnine-accept-completion-by-line)
         ("C-g" . tabnine-clear-overlay)
         ("M-[" . tabnine-previous-completion)
         ("M-]" . tabnine-next-completion)))

(use-package! company-quickhelp
  :after company
  :init
  (company-quickhelp-mode)
  :bind
  (:map company-active-map
        ("?" . company-quickhelp-manual-begin))
  :config
  (setq company-quickhelp-use-propertized-text t
        company-quickhelp-max-lines 10))
