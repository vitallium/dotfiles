;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      auth-sources '("~/.authinfo.gpg")

      doom-theme 'doom-dracula

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; Let me trigger the autocomplete
      company-idle-delay nil

      ;; More common use-case
      evil-ex-substitute-global t

      ;; make undo more fine-grained
      evil-want-fine-undo t

      projectile-project-search-path '("~/projects")

      avy-all-windows t)

(after! server
  (when (not (server-running-p))
    (server-start)))

(after! doom-themes
    (setq
        doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;;
;;; UI
(setq doom-font (font-spec :family "Agave" :size 20)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 20))

;; Maximize the window upon startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

(setq which-key-idle-delay 0.5) ;; I need the help, I really do
