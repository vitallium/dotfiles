;;; init-backup.el --- Backup configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Backup
(use-package files
  :elpaca nil
  :custom
  (backup-by-copying t)
  (backup-directory-alist (list (cons "." (file-name-concat user-emacs-cache-directory "backups"))))
  (delete-old-versions t)
  (version-control t)
  (make-backup-files t))

(provide 'init-backup)
;;; init-backup.el ends here