;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable package.el at startup
(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'default nil :height 170 :family "mononoki")
(set-face-attribute 'fixed-pitch nil :height 170 :family "mononoki")
(set-face-attribute 'variable-pitch nil :height 170 :family "Iosevka Etoile")

;; Maximize Emacs when it's opened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'early-init)
;;; early-init.el ends here.
