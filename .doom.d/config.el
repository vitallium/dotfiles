;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-theme 'doom-flatwhite
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; IMO, modern editors have trained a bad habit into us all: a burning
      ;; need for completion all the time -- as we type, as we breathe, as we
      ;; pray to the ancient ones -- but how often do you *really* need that
      ;; information? I say rarely. So opt for manual completion:
      company-idle-delay 0.3
      
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      ;;lsp-ui-sideline-enable nil
      ;;lsp-enable-symbol-highlighting nil
      
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
      doom-big-font (font-spec :family "Comic Mono" :size 34))

;; Fix cutoff of the modeline
(after! doom-modeline
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
(setq lsp-auto-guess-root nil                ; Causes problems esp. with golang projects misguessing the root.
      lsp-enable-symbol-highlighting nil     ; Lots of highlighting that is distracting.
      lsp-signature-auto-activate t          ; Show signature of current function.
      lsp-signature-render-documentation nil ; Only show single line of function.
      lsp-enable-snippet nil                 ; Disable auto parameter insertions.
      flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)) ; Restore lsp-mode flycheck behavior.

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;; Legacy stuff
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby-reek)))

(add-hook! org-mode-hook 'toc-org-mode)
