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
(after! apheleia
  (setf (alist-get 'elisp-mode apheleia-mode-alist)
        '(list-indent)))
