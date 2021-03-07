;; Enable mixed-pitch-mode for some text modes.

(add-hook! (org-mode gfm-mode markdown-mode) #'mixed-pitch-mode)
