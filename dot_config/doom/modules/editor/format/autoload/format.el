;;; editor/format/autoload/format.el -*- lexical-binding: t; -*-

(defun +format--use-lsp? (feature)
  "Return non-nil if we should use the LSP formatter for the specifi FEATURE."
  (and +format-with-lsp
       (bound-and-true-p lsp-mode)
       (lsp-feature? feature)))

;;;###autoload
(defun +format/buffer (&optional arg)
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive)
  (call-interactively
   (if (+format--use-lsp? "textDocument/formatting")
       #'lsp-format-buffer
     #'apheleia-format-buffer)))

;;;###autoload
(defun +format/region (beg end &optional arg)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (if (+format--use-lsp? "textDocument/rangeFormatting")
      (call-interactively #'lsp-format-region)
    (+format-region beg end)))

;;;###autoload
(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
 is selected)."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/region
     #'+format/buffer)))
