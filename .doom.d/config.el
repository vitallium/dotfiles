;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Vitaly Slobodin"
      user-mail-address "vslobodin@gitlab.com"
      doom-theme 'doom-gruvbox
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; IMO, modern editors have trained a bad habit into us all: a burning
      ;; need for completion all the time -- as we type, as we breathe, as we
      ;; pray to the ancient ones -- but how often do you *really* need that
      ;; information? I say rarely. So opt for manual completion:
      company-idle-delay nil
      
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      
      ;; More common use-case
      evil-ex-substitute-global t

      ;; make undo more fine-grained
      evil-want-fine-undo t

      ;;
      highlight-indent-guides-method 'bitmap)

;;
;;; UI
(setq doom-font (font-spec :family "hermit" :size 24)
      doom-variable-pitch-font (font-spec :family "Noto Sans")
      doom-big-font (font-spec :family "hermit" :size 34))

;; Enable mixed-pitch-mode for some text modes.
;; Also ensure it preserves the variable pitch height rather than inheriting from the default face.
(add-hook! (org-mode gfm-mode markdown-mode) #'mixed-pitch-mode)
(setq mixed-pitch-set-height t)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Configure centaur-tabs
;; (after! centaur-tabs
;;   (setq centaur-tabs-set-modified-marker t
;;         centaur-tabs-modified-marker "M"
;;         centaur-tabs-cycle-scope 'tabs
;;         centaur-tabs-set-close-button nil)
;;   (centaur-tabs-group-by-projectile-project))

;;
;;; Keybinds

(map!
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up
  "C-l" #'evil-window-right)

(map!
  "C-`" #'+popup/toggle
  "<C-tab>" #'+popup/other)

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

;;; :tools magit
(setq magit-repository-directories '(("~/source/" . 3))
      magit-save-repository-buffers nil
      magit-display-file-buffer-function #'switch-to-buffer-other-window
      ;; I want my stuff to clone to ~/source
      magithub-clone-default-directory "~/source"
      ;; HTTPS cloning is awful, i authenticate with ssh keys.
      magithub-preferred-remote-method 'ssh_url)  

;;; :lang org
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-startup-folded 'overview
      org-ellipsis " [...] "
      org-journal-dir "~/org/daily/"
      org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-find-file #'find-file-other-window
      org-journal-enable-agenda-integration t)

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

;; Legacy stuff
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby-reek)))

(add-hook!
 org-mode-hook 'toc-org-mode)

