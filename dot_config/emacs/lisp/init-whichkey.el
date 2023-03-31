;;; init-whichkey.el --- Which key configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-separator " ‧ ")
  (which-key-idle-delay 0.8)
  :config
  (which-key-mode))

(provide 'init-whichkey)
;;; init-whichkey.el ends here