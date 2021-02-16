;;; private/go/+hooks.el -*- lexical-binding: t; -*-

(add-hook! go-mode-hook #'gofmt-before-save)
