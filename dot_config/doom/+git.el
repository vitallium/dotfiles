;;; +git.el -*- lexical-binding: t; -*-

(after! smerge-mode
  (map! :mode smerge-mode-map :leader :desc "Git Select Other" "gdo" #'smerge-keep-other)
  (map! :mode smerge-mode-map :leader :desc "Git Keep Mine" "gdm"  #'smerge-keep-mine)
  (map! :mode smerge-mode-map :leader :desc "Git Keep All" "gda" #'smerge-keep-all)
  (map! :mode smerge-mode-map :leader :desc "Git Keep at cursor" "gdc" #'smerge-keep-current))

