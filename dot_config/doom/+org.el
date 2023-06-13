;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

;; Set the org-mode folder location
(setq org-directory "~/Documents/Org/")

(after! org
  (setq
   org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)" ))))

(set-popup-rule! "^\\*Org Agenda\\*$" :side 'right :size 0.4 :select t)

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("" "" "" "")))

(after! org-journal
  (setq org-journal-dir "~/Org/Journal"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"))

(add-hook! 'org-mode-hook
           #'+org-pretty-mode
           #'org-pretty-table-mode
           #'org-appear-mode
           #'org-modern-mode)
