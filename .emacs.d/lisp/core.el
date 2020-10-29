;;; core.el -*- lexical-binding: t; -*-

;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; Detect system type
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))
(defconst *IS-WINDOWS* (memq system-type '(cygwin windows-nt ms-dos)))
(defconst *IS-BSD*     (or *IS-MAC* (eq system-type 'berkeley-unix)))

(defvar bootstrap-version)
(defvar straight-base-dir)

;; Package manager
(setq straight-base-dir (expand-file-name "emacs/" (xdg-data-home)))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'core)

;;; core.el ends here
