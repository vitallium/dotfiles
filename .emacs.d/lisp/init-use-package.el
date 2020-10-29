(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; To not increase Emacs startup time, check package modifications when packages
;; edited (with Emacs) or manually invoke =straight-check-all= command, instead of
;; checking modifications at startup.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Tell straight to follow symlinks
(setq vc-follow-symlinks t)

(provide 'init-use-package)
