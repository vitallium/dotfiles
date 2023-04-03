;;; $DOOMDIR/+tabnine.el -*- lexical-binding: t; -*-

(use-package! company-tabnine
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (setq
   ;; Set the trigger length (the default is 3)
   company-tabnine-trigger t
   ;; Enable TabNine on startup
   company-tabnine--disable-auto-shutdown t
   ;; Use TabNine in addition to the other backends
   company-idle-delay 0
   company-show-quick-access t))
