;;; $DOOMDIR/+editor.el -*- lexical-binding: t; -*-

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Activate bug-reference-prog-mode
(after! prog-mode
  (bug-reference-prog-mode))

;; Indent guides
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-auto-odd-face-perc 3
        highlight-indent-guides-auto-even-face-perc 1.5))

;; Setup apheleia
(use-package! apheleia
  :hook ((emacs-lisp-mode . apheleia-mode)
         (tsx-mode . apheleia-mode)
         (typescript-mode . apheleia-mode)
         (js-mode . apheleia-mode)
         (json-mode . apheleia-mode)
         (css-mode . apheleia-mode)
         (scss-mode . apheleia-mode)
         (go-mode . apheleia-mode))
  :defer t
  :config
  (push '(tsx-mode . prettier) apheleia-mode-alist)
  (push '(scss-mode . prettier) apheleia-mode-alist)
  (push '(css-mode . prettier) apheleia-mode-alist)
  (push '(emacs-lisp-mode . lisp-indent) apheleia-mode-alist))
