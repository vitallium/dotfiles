(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

(map! :leader
      (:prefix-map ("TAB" . "workspace")
       :desc "Switch to last workspace" "," #'+workspace/other)
      :prefix "n"
      "c" #'org-capture)

(map! "C-x b"   #'counsel-buffer-or-recentf
      "C-x C-b" #'counsel-switch-buffer
      "C-s"     #'+default/search-buffer
      "M-g g"   #'avy-goto-line
      "M-g M-g" #'avy-goto-line
      "M-g o" #'counsel-outline
      "M-p" #'ace-window)

(map! "s-g" #'magit-status
      "C-c g" #'magit-status
      "s-G" #'magit-blame-addition
      "C-c G" #'magit-blame-addition)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))
