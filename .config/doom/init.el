;;; init.el -*- lexical-binding: t; -*-

(setq evil-disable-insert-state-bindings t)

(doom!
 :completion
 (company          ; the ultimate code completion backend
  +childframe)
 (vertico +icons)
 
 :ui
 doom              ; what makes DOOM look the way it does
 doom-dashboard    ; a nifty splash screen for Emacs
 (emoji +unicode)  ; 🙂
 hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 (:if (display-graphic-p)
     (ligatures
      +extra))    ; ligatures and symbols to make your code pretty again
 modeline          ; snazzy, Atom-inspired modeline, plus API
 ophints           ; highlight the region an operation acts on
 (popup +defaults)   ; tame sudden yet inevitable temporary windows
 (vc-gutter +pretty) ; vcs diff in the fringe
 vi-tilde-fringe   ; fringe tildes to mark beyond EOB
 workspaces        ; tab emulation, persistence & separate workspaces
 zen               ; distraction-free coding or writing
 
 :editor
 (evil +everywhere +hybrid) ; come to the dark side, we have cookies
 file-templates     ; auto-snippets for empty files
 fold               ; (nigh) universal code folding
 (format +onsave) ;; when commented out, $DAYJOB has too many badly-formatted files
 ;;format ; automated prettiness
 snippets           ; my elves. They type so I don't have to
 
 :emacs
 undo                ; persistent, smarter undo for your inevitable mistakes
 vc                  ; version-control and Emacs, sitting in a tree
 (dired +dirvish +icons)               ; making dired pretty [functional]
 
 :term
 vterm             ; the best terminal emulation in Emacs
 
 :checkers
 syntax            ; tasing you for every semicolon you forget
 
 :tools
 (debugger +lsp)     ; FIXME stepping through code, to help you add bugs
 direnv
 docker
 editorconfig      ; let someone else argue about tabs vs spaces
 (eval +overlay)     ; run code, run (also, repls)
 (lookup +docsets)   ; navigate your code and its documentation
 (lsp +peek)
 (magit
 +forge)          ; a git porcelain for Emacs
 make              ; run make tasks from Emacs
 pdf               ; pdf enhancements
 tmux              ; an API for interacting with tmux
 tree-sitter       ; syntax and parsing, sitting in a tree...
 
 :os
 (:if IS-MAC macos)  ; improve compatibility with macOS
 tty               ; improve the terminal Emacs experience
 
 :lang
 (clojure +lsp +treesitter)           ; java with a lisp
 data              ; config/data formats
 (elixir +lsp)            ; erlang done right
 emacs-lisp        ; drown in parentheses
 (json +lsp)              ; At least it ain't XML
 (javascript
   +lsp
   +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
 (lua
   +fennel
   +lsp)                 ; one-based indices? one-based indices
 markdown            ; writing docs for people to ignore
 (org                ; organize your plain life in plain text
   +pretty
   +dragndrop
   +present
   +gnuplot
   +roam2)
 (python             ; beautiful is better than ugly
   +lsp
   +pyright
   +tree-sitter)
 rest              ; Emacs as a REST client
 (ruby
   +rails
   +rbenv
   +lsp
   +tree-sitter)        ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
 (scheme +guile)   ; a fully conniving family of lisps
 
 (sh
   ;; +lsp
   +tree-sitter)        ; she sells {ba,z,fi}sh shells on the C xor
 (web
   +lsp
   +tree-sitter)        ; the tubes
 (yaml
   + lsp
   +tree-sitter)        ; JSON, but readable
 (janet +tree-sitter)
 
 :config
 literate
 (default +bindings +smartparens))
