;;; editor/format/autoload.el -*- lexical-binding: t; -*-

;; Format a specific region
(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

(defun +format-region (start end &optional callback)
  "Format from START to END with `apheleia'."
  (when-let* ((command (apheleia--get-formatter-command
                        (if current-prefix-arg
                            'prompt
                          'interactive)))
              (cur-buffer (current-buffer))
              (formatted-buffer (get-buffer-create " *apheleia-formatted*"))
              (indent 0))
    (with-current-buffer formatted-buffer
      (erase-buffer)
      (setq-local coding-system-for-read 'utf-8)
      (setq-local coding-system-for-write 'utf-8)
      ;; Ensure this temp buffer seems as much like the origin buffer as
      ;; possible, in case the formatter is an elisp function, like `gofmt'.
      (cl-loop for (var . val)
               in (cl-remove-if-not #'listp (buffer-local-variables origin-buffer))
               ;; Making enable-multibyte-characters buffer-local causes
               ;; an error.
               unless (eq var 'enable-multibyte-characters)
               do (set (make-local-variable var) val))
      (insert-buffer-substring-no-properties cur-buffer start end)
      ;; Since we're piping a region to the formatter, remove any leading
      ;; indentation to make it look like a file.
      (setq indent (+format--current-indentation))
      (when (> indent 0)
        (indent-rigidly (point-min) (point-max) (- indent)))
      (apheleia-format-buffer
       command
       (lambda ()
         (with-current-buffer formatted-buffer
           (when (> indent 0)
             ;; restore indentation without affecting new indentation
             (indent-rigidly (point-min) (point-max)
                             (max 0 (- indent (+format--current-indentation))))))
         (with-current-buffer cur-buffer
           (delete-region start end)
           (insert-buffer-substring-no-properties formatted-buffer)
           (when callback (funcall callback))
           (kill-buffer formatted-buffer)))))))
