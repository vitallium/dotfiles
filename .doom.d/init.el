;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion
       (company +childframe)           ; the ultimate code completion backend
       (ivy +prescient
            +icons)    ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       indent-guides     ; highlighted indent columns
       modeline         ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +all +defaults)   ; tame sudden yet inevitable temporary windows
       ;; tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +ranger
              +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       (undo +tree)              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       (cc                 ; C/C++/Obj-C madness
        +lsp)
       ;;clojure           ; java with a lisp
       ;;(common-lisp +lsp)       ; if you've seen one lisp, you've seen them all
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;(erlang +lsp)           ; an elegant language for a more civilized age
       (go                 ; the hipster dialect
        +lsp)
       ;;(haskell +dante)  ; a language that's lazier than I am
       json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript         ; all(hope(abandon(ye(who(enter(here))))))
        +lsp)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       (markdown +grip)          ; writing docs for people to ignore
       (nix +lsp)               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +journal
            +roam)               ; organize your plain life in plain text
       ;;plantuml          ; diagrams for confusing people more
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest                ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby               ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +lsp
        +rails)
       rust             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh +fish)               ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)               ; the tubes
       yaml              ; JSON, but readable

       :email
       (mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar

       :private
       ;; completion
       company
       ;; ui
       modeline
       ;; lang
       js
       go
       org
       ruby
       ;; tools
       lsp
       magit

       :config
       (default +bindings +smartparens))
