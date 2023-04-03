;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

(setq org-directory "~/Org")

(set-popup-rule! "^\\*Org Agenda\\*$" :side 'right :size 0.4 :select t)

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("" "" "" "")))

(after! org-journal
  (setq org-journal-dir "~/Org/Journal"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"))

(use-package! org-ol-tree
  :after org
  :config
  (add-hook 'org-agenda-mode #'org-super-agenda-mode))

(add-hook! 'org-mode-hook
  #'mixed-pitch-mode
  #'+org-pretty-mode
  #'org-pretty-table-mode
  #'org-appear-mode
  #'org-modern-mode)
