;;; $DOOMDIR/+yaml.el -*- lexical-binding: t; -*-

(use-package! yaml-pro
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-mode)
  :config
  (map! :map yaml-pro-mode-map
       [remap imenu] #'yaml-pro-jump
        :n "zc" #'yaml-pro-fold-at-point
        :n "zo" #'yaml-pro-unfold-at-point
        :n "gk" #'yaml-pro-prev-subtree
        :n "gj" #'yaml-pro-next-subtree
        :n "gK" #'yaml-pro-up-level
        :n "<M-down>" #'yaml-pro-move-subtree-down
        :n "<M-up>" #'yaml-pro-move-subtree-up
        :n "M-k" #'yaml-pro-move-subtree-up
        :n "M-j" #'yaml-pro-move-subtree-down))
