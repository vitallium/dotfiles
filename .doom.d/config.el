;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-theme 'doom-dracula
      doom-localleader-key ","
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      company-idle-delay nil

      ;; More common use-case
      evil-ex-substitute-global t

      ;; make undo more fine-grained
      evil-want-fine-undo t

      ;;
      highlight-indent-guides-method 'bitmap)

;;
;;; UI
(setq doom-font (font-spec :family "mononoki" :size 18)
      doom-variable-pitch-font (font-spec :family "mononoki" :size 18)
      doom-big-font (font-spec :family "Comic Mono" :size 20))

;; Global settings (defaults)

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

;; Enable mixed-pitch-mode for some text modes.
;; Also ensure it preserves the variable pitch height rather than inheriting from the default face.
(add-hook! (org-mode gfm-mode markdown-mode) #'mixed-pitch-mode)
(setq mixed-pitch-set-height t)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;
;;; Keybindings

(map! :leader
      (:prefix-map ("TAB" . "workspace")
       :desc "Switch to last workspace" "," #'+workspace/other))

(map! :leader
      (:prefix "c"
       :desc "LSP Parameters" "p" #'lsp-signature-activate))

;;       (:leader
;;        "x" nil ;; Disable x prefix for scratch buffer
;;        (:prefix "a"
;;         :desc "GTD" :nv "g" #'org-agenda-gtd))

;;       (:prefix ("x" . "text-transform")
;;        (:prefix ("f" . "copy-as-format")
;;         :desc "Markdown" :nv "m" #'copy-as-format-markdown
;;         :desc "Slack" :nv "s" #'copy-as-format-markdown)))

;;
;;; Modules

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; ZEN
(after! writeroom-mode
  (setq +zen-text-scale 0
        +zen-mixed-pitch-modes nil
        writeroom-mode-line t
        writeroom-width 160))

;;; :tools treemacs
;;; Make cursor follow to the buffer file.
(after! treemacs
  (treemacs-follow-mode 1))

;;; workspaces
;;; Always open up a new workspace when opening up a project.
(setq +workspaces-on-switch-project-behavior t)

;;; :tools mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a")

;; Legacy stuff
(add-hook! org-mode-hook 'toc-org-mode)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook! 'after-init-hook #'mu4e-alert-enable-notifications)
(after! doom-modeline
  (mu4e-alert-enable-mode-line-display))

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
