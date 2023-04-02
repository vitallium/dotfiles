;;; init-emacs.el --- General emacs configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-data-directory))
(load custom-file 'noerror)

;; Misc. config
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq help-window-select t)
(setq read-process-output-max (* 1024 1024 8)) ; Increase amount of data emacs can read from child processes to 8mb

;; Disable native comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

;; isearch tweaks
(setq-default
     isearch-allow-scroll t
     lazy-highlight-cleanup nil
     lazy-highlight-initial-delay 0
	   lazy-highlight-buffer t)

;; Highlight current line
(global-hl-line-mode 1)

;; Simplify yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll like a normal editor
(setq scroll-margin 8
	  scroll-conservatively 101
	  scroll-up-aggressively 0.01
	  scroll-down-aggressively 0.01
	  scroll-preserve-screen-position t
	  auto-window-vscroll nil)

;; Smooth scrolling
(setq pixel-scroll-precision-use-momentum t)
(pixel-scroll-precision-mode)

;; Tab width
(setq-default tab-width 4)

;; Auto close brackets
(electric-pair-mode)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Autocomplete word
(global-set-key (kbd "M-/") 'hippie-expand)

;; Compilation scrolling
(setq compilation-scroll-output 'first-error)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
	  #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Don't want ESC as a modifier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package diminish
  :config
  (diminish 'global-whitespace-mode))

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :hook (emacs-startup . (lambda ()
                           (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
                           (exec-path-from-shell-initialize))))

;; Disable frame suspension
(unbind-key "C-z")
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; TODO: 
(use-package s
  :commands (s-trim)
  :init
  (setenv "SSH_AUTH_SOCK" (s-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))

(provide 'init-emacs)
;;; init-emacs.el ends here
