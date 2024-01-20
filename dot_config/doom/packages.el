;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)

(package! git-link)
(package! eredis)

;; Auto-complete
(package! company-quickhelp)
(package! tabnine)

;; Themes
(package! modus-themes)
(package! ef-themes)
(package! standard-themes)

(package! visual-fill-column)
(package! string-inflection)

;; Org mode goodies
(package! org-modern)
(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! mixed-pitch)
(package! mermaid-ts-mode :recipe (:host github :repo "JonathanHope/mermaid-ts-mode"))

;; tree-sitter
(package! treesit-auto :disable t)
(package! ts-fold :recipe (:host github :repo "emacs-tree-sitter/ts-fold"))

;; Improve YAML editing
(package! yaml-pro)

;; Replace json-mode with jsonian
(package! json-mode :disable t)
(package! jsonian :recipe (:host github :repo "iwahbe/jsonian"))
(package! jsonnet-mode)

(package! multi-vterm)

;; Ruby
(package! rbs-mode)
(package! ruby-refactor)
(package! yari)

;; Frontend
(package! jest-test-mode)
(package! js-doc)

;; mise (ex-rtx) integration
(package! mise :recipe (:host github :repo "vitallium/mise.el"))

;; Embark integration for vc
(package! embark-vc)

;; Automatically toggle between light and dark theme
(package! auto-dark)

(package! fish-completion :pin "d34d0b96fde63feedf13c4288183d8d4d4d748cf")
