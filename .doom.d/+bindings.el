;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map! "<f5>" #'modus-themes-toggle)

(map! :leader
      :desc "Auto fill"
      :n "t a" 'auto-fill-mode)

(map! :leader
      :desc "Org Project Capture"
      :n "n p" 'org-projectile-project-todo-completing-read)

(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

(use-package avy
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC k l" . avy-kill-whole-line)
         ("SPC k r" . avy-kill-region))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))
