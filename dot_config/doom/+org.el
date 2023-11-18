;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

;; Set the org-mode folder location
(setq org-directory "~/Documents/Org/"
      org-agenda-files (list
                        "~/org/"
                        "\.org$")
      org-archive-location (concat org-directory ".archive/%s::")
      org-use-property-inheritance t)

(after! org
  (setq org-log-into-drawer 't
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)"))))

(set-popup-rule! "^\\*Org Agenda\\*$" :side 'right :size 0.4 :select t)

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("" "" "" "")))

(after! org-journal
  (setq org-journal-dir "~/Org/Journal"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-find-file #'find-file-other-window))

(map! :leader :desc "Open today's journal" "j" #'org-journal-open-current-journal-file)

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.3))

(add-hook! 'org-mode-hook
           #'+org-pretty-mode
           #'mixed-pitch-mode
           #'org-pretty-table-mode
           #'org-modern-mode)

(setq +zen-mixed-pitch-modes '(org-mode markdown-mode gfm-mode Info-mode rst-mode adoc-mode))

(dolist (hook +zen-mixed-pitch-modes)
  (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode))
