;;; private/org/config.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-file-format "%Y%m%d.org"
      org-startup-folded 'overview
      org-ellipsis " [...] "
      org-journal-dir "~/org/daily/"
      org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-find-file #'find-file-other-window
      org-journal-enable-agenda-integration t)

(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gtd.org"
                         "~/org/tickler.org"
                         "~/org/gitlab.org"))

;; Capturing a thought is one key press away:
;; simply Press C-c c, and a capture popup will appear in Emacs.
;; Once you’re done capturing, C-c C-c and it will get stored in the inbox.
;;
;; I press C-c c t to add an entry to my inbox, and C-c c T to add an entry to the tickler.
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/ord/tickler.org" "Tickler")
                               "* %i%? \n %U")))

;; My inbox is then processed and emptied daily.
;; When processing the inbox, I refile each entry that is actionable
;; and belongs to a project using C-c C-w,
;; moving the entry to the appropriate place.
;; If need be, I create a new project out of it.
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))

;; I put a todo keyword in all project entries.
;; I think I use fairly regular todo keywords: TODO, WAITING, DONE and CANCELLED mostly.
;; The first two for are used for incomplete states, and the last two for completed states.
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
