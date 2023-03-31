;;; init-lang-docker.el --- Control docker. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dockerfile-mode
  :diminish
  :defer t)

(use-package docker
  :elpaca (:host github :repo "Silex/docker.el")
  :defer t
  :bind ("C-c d" . docker))

(provide 'init-lang-docker)
;;; init-lang-docker.el ends here