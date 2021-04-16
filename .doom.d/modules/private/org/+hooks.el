;; Enable mixed-pitch-mode for some text modes.
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

(add-hook! 'org-mode-hook #'mixed-pitch-mode)
