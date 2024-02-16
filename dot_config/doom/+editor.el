;;; $DOOMDIR/+editor.el -*- lexical-binding: t; -*-

;; Visual Fill Column
(setq-default fill-column 120)
(setq fill-column 120
      visual-fill-column-center-text t
      visual-fill-column-width fill-column)

;; don't enable smartparens by default - when it doesn't work, it's really frustrating
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(add-hook! 'prog-mode-hook
           ;; Activate bug-reference-prog-mode
           #'bug-reference-prog-mode
           #'rainbow-delimiters-mode)
;;#'display-fill-column-indicator-mode)

;; Setup apheleia
(after! apheleia
  (setf (alist-get 'elisp-mode apheleia-mode-alist)
        '(lisp-indent))
  ;; Configure rubocop
  (setf (alist-get 'rubocop apheleia-formatters)
        '("rubocop" "--stdin" filepath "--autocorrect"
          "--stderr" "--format" "quiet" "--fail-level" "fatal"))
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(rubocop)))

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))
