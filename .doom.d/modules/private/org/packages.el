;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

; GitLab integration for the org-mode
(package! org-gitlab :recipe (:host gitlab :repo "to1ne/org-gitlab"))

(package! org-super-agenda)
(package! org-fancy-priorities)
(package! org-journal)
(package! toc-org)
