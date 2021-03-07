;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-theme 'doom-dracula
      doom-localleader-key ","

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; Let me trigger the autocomplete
      company-idle-delay nil

      ;; More common use-case
      evil-ex-substitute-global t

      ;; make undo more fine-grained
      evil-want-fine-undo t

      projectile-project-search-path '("~/projects"))

;;
;;; UI
(setq doom-font (font-spec :family "mononoki" :size 18)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 20))

;; Maximize the window upon startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; This block and the ones below exist so that the
;; doom theme is loaded and applied correctly to the frame
;; as without all of this, the theme doesn't get loaded
;; when using emacs --daemon
;; Fix from: https://github.com/hlissner/emacs-doom-themes/issues/125#issuecomment-372509034
;; More info: https://github.com/hlissner/emacs-doom-themes/issues/125
(defun doom|init-theme ()
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme doom-theme t))

(defun doom|init-theme-in-frame (frame)
  (with-selected-frame frame
    (doom|init-theme))

  ;; Unregister this hook once its run
  (remove-hook 'after-make-frame-functions
               'doom|init-theme-in-frame))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              'doom|init-theme-in-frame)
  (doom|init-theme))
;; The modeline is not useful to me in the popup window. It looks much nicer
;; to hide it
(add-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

;; Global settings (defaults)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(load! "+bindings")

;;
;;; Modules

;;; workspaces
;;; Always open up a new workspace when opening up a project.
(setq +workspaces-on-switch-project-behavior t)

;; Legacy stuff
(add-hook! org-mode-hook 'toc-org-mode)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

;; testing
(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(custom-theme-set-faces! 'doom-dracula
 `(markdown-code-face :background ,(doom-darken 'bg 0.075))
 `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

(setq which-key-idle-delay 0.5) ;; I need the help, I really do
