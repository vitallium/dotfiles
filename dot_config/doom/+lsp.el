;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages)
  ;; Delete the stupid vue-semantic-server which takes over all buffers
  (delete 'lsp-volar lsp-client-packages)

  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)

  ;; Add "tmp" and ".devbox" directories to ignored list of directories.
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.devbox\\'")

  (after! lsp-ruby-lsp
    (add-to-list 'lsp-ruby-lsp-library-directories "~/.local/share/mise/installs/ruby"))

  (setq lsp-json-schemas
        `[
          ;; From https://github.com/b0o/SchemaStore.nvim/blob/main/lua/schemastore/catalog.lua#L2796
          (:fileMatch [".prettierrc" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml"] :url "https://json.schemastore.org/prettierrc.json")
          ])

  ;; https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))

(after! lsp-ui
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-show-with-mouse t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-use-childframe nil
   lsp-ui-doc-use-webkit t
   ;; for some reason doom sets these too small
   lsp-ui-doc-max-height 15
   lsp-ui-doc-max-width 150))
