;;; private/checkers/spell/config.el -*- lexical-binding: t; -*-

(after! flyspell
  (setq flyspell-correct-popup t))

(after! ispell
  (setq ispell-program-name (executable-find "hunspell"))
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (setq ispell-dictionary "en_US,ru_RU"))

(add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "en_US"))
