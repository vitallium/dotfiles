;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/Org"
        org-agenda-files '(
                           "~/Org/private.org"
                           "~/Org/work.org"
                           "~/Org/inbox.org"
                           "~/Org/misc.org"
                           )
        +org-capture-todo-file "~/Org/inbox.org"
        org-log-done 'time)

  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
                            (type     "PROJ(p)" "ISSUE(i)" "BUG(b)" "REVIEW(r)" "|" "SOLVED(s)")))

  (setq org-archive-location (concat org-directory ".archive/%s::")
        org-refile-targets '(("~/Org/private.org" :maxlevel . 3)
                             ("~/Org/work.org" :maxlevel . 2)
                             ("~/Org/misc.org" :maxlevel . 2)))

  (setq org-capture-templates
        '(
          ("t" "todo" entry (file +org-capture-todo-file) "* TODO %?")
          ("p" "process email" entry (file +org-capture-todo-file)
               "* TODO %? %:fromname: %a")))

  (set-popup-rule! "^\\*Org Agenda\\*$" :side 'right :size 0.4 :select t)

  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (+org-pretty-mode)
  (org-pretty-table-mode)
  (global-org-modern-mode))

(after! org-journal
  (setq org-journal-dir "~/Journal"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"))

(use-package org-ol-tree
  :after org
  :config
  (add-hook 'org-agenda-mode #'org-super-agenda-mode))

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook #'org-appear-mode))
