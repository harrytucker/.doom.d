;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :completion
       (corfu +icons +dabbrev)      ; complete with cap(f), cape and a flying feather!
       (vertico +icons +childframe) ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       minimap
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       (treemacs +lsp)   ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;tabs            ; an tab bar for Emacs

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       (format +onsave)  ; automated prettiness
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       multiple-cursors  ; editing in many places at once
       ;; parinfer          ; turn lisp into python, sort of (disabled as currently not working on ARM)
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       (ibuffer +icons)  ; interactive buffer management
       (undo +tree)      ; undoing stuff with a tree looks cool
       electric          ; smarter, keyword-based electric-indent
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell
       vterm             ; another terminals in Emacs

       :checkers
       syntax            ; tasing you for every semicolon you forget

       :tools
       (eval +overlay)   ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       docker            ; thar she blows
       (lsp +peek)       ; integrate with *every* language
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       upload            ; map local to remote projects via ssh/ftp
       tree-sitter
       direnv

       :os
       (:if (featurep :system 'macos) macos) ; improve compatibility with macOS

       :lang
       (go               ; the hipster dialect
        +lsp
        +tree-sitter)
       (rust             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp
        +tree-sitter)
       (python           ; beautiful is better than ugly
        +pyright
        +poetry
        +lsp
        +tree-sitter)
       (javascript       ; all(hope(abandon(ye(who(enter(here))))))
        +lsp             ; enable lsp support
        +tree-sitter)    ; better syntax highlighting
       (web              ; the tubes
        +lsp
        +tree-sitter)
       (zig              ; C, but simpler
        +lsp
        +tree-sitter)
       (java             ; the poster child for carpal tunnel syndrome
        +meghanada
        +tree-sitter)
       (haskell          ; a language that's lazier than I am
        +lsp
        +tree-sitter)
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +hugo            ; use Emacs for hugo blogging
        +pandoc          ; export-with-pandoc support
        +pomodoro        ; be fruitful with the tomato technique
        +roam2           ; enable org-roam v2
        +present)        ; using org-mode for presentations
       (json
        +tree-sitter)    ; no trailing commas for you
       (sh               ; she sells {ba,z,fi}sh shells on the C xor
        +tree-sitter)
       (yaml             ; JSON, but readable
        +lsp
        +tree-sitter)
       emacs-lisp        ; drown in parentheses
       data              ; config/data formats
       (markdown +grip)  ; writing docs for people to ignore
       nim               ; python + lisp at the speed of c
       (rest +jq)        ; Emacs as a REST client
       (cc
        +lsp
        +tree-sitter)
       (nix
        +lsp
        +tree-sitter)

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;emms
       ;;everywhere      ; *leave* Emacs!? You must be joking
       ;;irc             ; how neckbeards socialize
       ;;(rss +org)      ; emacs as an RSS reader
       ;;twitter         ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))
