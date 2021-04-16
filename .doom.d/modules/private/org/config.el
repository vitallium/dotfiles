;;; private/org/config.el -*- lexical-binding: t; -*-

(doom-themes-org-config)

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

(setq org-agenda-files (file-expand-wildcards "~/org/gtd/*.org"))
(setq global-org-pretty-table-mode t)

(setq mixed-pitch-variable-pitch-cursor nil)

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "REVIEW(r)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)" "MERGED(m)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(after! org-capture
  (setq org-capture-templates
    '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")"* TODO %?\n  %i\n  %a")
      ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n%i\n%a" :prepend t)
      ("j" "Journal templates")
      ("je" "Entry" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format)%^{Title} %^g\n%i%?")
      ("jt" "Todo" plain (function org-journal-find-location) "** TODO %^{Title} %^g\n%i%?")
      ("o" "Centralized templates for projects")
      ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
      ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
      ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (customize-set-variable 'org-journal-file-format "%Y-%m-%d.org")
  (customize-set-variable 'org-journal-date-format "%Y-%m-%d %A")
  (setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"REVIEW\""))

(load! "+hooks")
