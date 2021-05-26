;; Enable mixed-pitch-mode for some text modes.
(add-hook! 'org-mode-hook
           #'+org-pretty-mode #'mixed-pitch-mode)
