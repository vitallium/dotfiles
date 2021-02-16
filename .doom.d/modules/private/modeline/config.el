;;; private/modeline/config.el -*- lexical-binding: t; -*-

(defun doom-modeline-conditional-buffer-encoding ()
  "Only show text encoding when it's not UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(after! doom-modeline
  (setq
   doom-modeline-project-detection `projectile
   doom-modeline-checker-simple-format t
   doom-modeline-buffer-file-name-style 'truncate-upto-root
   doom-modeline-workspace-name t
   doom-modeline-persp-name nil
   doom-modeline-persp-icon nil
   doom-modeline-mu4e t))

(provide 'config)
