;;; tools/just/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "just")
  (warn! "Couldn't find just."))
