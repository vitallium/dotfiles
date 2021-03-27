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
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "STORY(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(load! "+hooks")
