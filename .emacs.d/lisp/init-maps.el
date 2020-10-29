;;; init-maps --- Key mappings

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-separator " "
	which-key-prefix-prefx "+")
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

;; Defines
(defvar key-leader "SPC"
  "<leader> key for Evil mode.")
(defvar key-non-leader "M-c"
  "<leader> key for insert and Emacs states.")
(defvar key-local-leader "M-m"
  "Prefix for bindings specific for current major mode.")

;; Buffers
(general-define-key
 :states '(normal insert emacs)
 :prefix key-leader
 :non-normal-prefix key-non-leader
 "b" '(:ignore t :wk "Buffers")
 "b b" '(switch-to-buffer :wk "Switch to buffer")
 "b k" '(kill-this-buffer :wf "Kill this buffer"))

;; Files
(general-define-key
 :states '(normal insert emacs)
 :prefix key-leader
 :non-normal-prefix key-non-leader
 "f" '(:ignore t :wk "Files")
 "f f" '(find-file :wk "Find file"))

;; Projects
(general-define-key
 :states '(normal insert emacs)
 :prefix key-leader
 :non-normal-prefix key-non-leader
 "p" '(:ignore t :wk "Projects")
 "p p" '(projectile-switch-project :wk "Switch project")
 "p a" '(projectile-add-known-project :wk "Add project")
 "p f" '(projectile-find-file :wk "Find file"))

;; Windows
(general-define-key
 :states '(normal insert emacs)
 :prefix key-leader
 :non-normal-prefix key-non-leader
 "w" '(:ignore t :wk "Windows")
 "w h" '(evil-window-left :wk "Left window")
 "w j" '(evil-window-down :wk "")
 "w k" '(evil-window-up :wk "Add project")
 "w l" '(evil-window-right :wk "Add project")
 "w s" '(split-window-vertically :wk "Split vertically")
 "w v" '(split-window-horizontally :wk "Split horizontally")
 "w c" '(delete-window :wk "Close this split")
 "w =" '(balance-windows :wk "Balance windows"))

;; Magit
(general-define-key
 :states '(normal insert emacs)
 :prefix key-leader
 :non-normal-prefix key-non-leader
 "g" '(:ignore t :wk "Magit")
 "g s" '(magit-status :wk "Magit status"))
 
(provide 'init-maps)

;;; init-maps.el ends here
