;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-theme 'doom-dracula
      doom-localleader-key ","
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      company-idle-delay 0.3

      ;; More common use-case
      evil-ex-substitute-global t

      ;; make undo more fine-grained
      evil-want-fine-undo t

      ;;
      highlight-indent-guides-method 'bitmap)

;;
;;; UI
(setq doom-font (font-spec :family "Hermit" :size 24)
      doom-variable-pitch-font (font-spec :family "Comic Mono")
      doom-big-font (font-spec :family "Comic Mono" :size 18))

;; Fix cutoff of the modeline
(after! doom-modeline
  (setq doom-modeline-mu4e t)
      (doom-modeline-def-modeline 'main
        '(bar matches buffer-info remote-host buffer-position parrot selection-info)
        '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")))

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

(map! :nv "C-S-k" #'move-line-up
       :nv "C-S-j" #'move-line-down)

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

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

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

;;; :tools lsp
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-eldoc-enable-hover nil
        lsp-diagnostics-provider 'flycheck))

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 1))

;;; :tools mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(setq mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a")

;; Legacy stuff
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby-reek)))

(add-hook! org-mode-hook 'toc-org-mode)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(custom-theme-set-faces! 'doom-dracula
  `(markdown-code-face :background ,(doom-darken 'bg 0.075))
  `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))


(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
