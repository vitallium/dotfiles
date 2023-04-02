;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Adjust garbage collector thresholds
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.
In a nutshell, it's just a value of $HOME.")

(defconst path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst user-emacs-data-directory
  (concat
   (file-name-as-directory
    (or (getenv "XDG_DATA_HOME")
        (concat path-home-dir ".local/share")))
   "emacs/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defconst user-emacs-cache-directory
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat path-home-dir ".cache")))
   "emacs/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management
(require 'init-elpaca)
;; Core configuration. Keep above all other modules.
(require 'init-emacs)
;; (require 'init-execpath)

;; Order does not matter
(require 'init-alltheicons)
(require 'init-apheleia)
(require 'init-backup)
(require 'init-buffers)
(require 'init-completion)
(require 'init-consult)
(require 'init-dired)
(require 'init-direnv)
(require 'init-eglot)
(require 'init-flycheck)
(require 'init-helpful)
(require 'init-magit)
(require 'init-perspective)
(require 'init-projectile)
(require 'init-theme)
(require 'init-treesitter)
(require 'init-vterm)
(require 'init-whichkey)
(require 'init-window)

;; Languages
(require 'init-lang-docker)
(require 'init-lang-go)
(require 'init-lang-ruby)
(require 'init-lang-yaml)

(provide 'init)
;; init.el ends here.
