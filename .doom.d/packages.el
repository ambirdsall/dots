;; -*- no-byte-compile: t; -*-

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! benchmark-init)

;; * UI
(package! emojify)

;; * org-mode
(package! mixed-pitch)

(package! outshine
  :recipe (:host github :repo "alphapapa/outshine"))
(package! graphviz-dot-mode)
(package! ox-gfm)

;; * there are more languages than :lang can speak
;; ** language modes
(package! fennel-mode)
(package! graphql-mode)

;; ** lsp
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! evil-tmux-navigator
  :recipe (:host github :repo "ambirdsall/evil-tmux-navigator"))
(unpin! evil-tmux-navigator)

(package! evil-replace-with-register)

(package! evil-exchange)

(package! evil-matchit)

(package! evil-textobj-line
  :recipe (:host github :repo "emacsorphanage/evil-textobj-line"))

(package! evil-snipe :disable t)

(package! code-compass
  :recipe (:host github :repo "ag91/code-compass" :files (:defaults "pages" "scripts")))
