;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(map! :leader
      :desc "Auto fill"
      :n "t a" 'auto-fill-mode)

(map! :leader :desc "Find file in other window" "fo" #'find-file-other-window)

(map! :g [mouse-8] #'better-jumper-jump-backward
      :g [mouse-9] #'better-jumper-jump-forward)

(after! avy
  (map! :nvm "s" #'avy-goto-char-timer)
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(map!
 (:when (modulep! :completion vertico)
   (:after vertico
    :map vertico-map
    (:when (modulep! :editor evil +everywhere)
      "C-h"  #'vertico-directory-up
      "C-l"  #'vertico-directory-enter))))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))
