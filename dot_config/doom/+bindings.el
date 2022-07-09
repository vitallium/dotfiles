;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map! "<f5>" #'modus-themes-toggle)

(map! :leader
      :desc "Auto fill"
      :n "t a" 'auto-fill-mode)

(map! :leader
      :desc "Org Project Capture"
      :n "n p" 'org-projectile-project-todo-completing-read)

(after! avy
  (map! :nvm "s" #'avy-goto-char-timer)
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package! transpose-frame
  :config
  (map! :leader
        :prefix ("r" . "rotate")
        :desc "Transpose frames" "t" #'transpose-frame
        :desc "Flop frames horizontally" "f" #'flop-frame
        :desc "Flip frames vertically" "v" #'flip-frame
        :desc "Rotate frames 180 degrees" "r" #'rotate-frame
        :desc "Rotate frames clockwise" "c" #'rotate-frame-clockwise
        :desc "Rotate frames anticlockwise" "a" #'rotate-frame-anticlockwise))

(map! :leader :desc "Find file in other window" "fo" #'find-file-other-window)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
