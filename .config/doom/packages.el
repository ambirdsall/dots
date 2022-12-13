;; -*- no-byte-compile: t; -*-

(package! benchmark-init)

(package! emojify)

(package! fennel-mode)

(package! graphql-mode)

(package! apheleia)

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! code-compass
  :recipe (:host github :repo "ag91/code-compass" :files (:defaults "pages" "scripts")))

(package! evil-tmux-navigator
  :recipe (:host github :repo "ambirdsall/evil-tmux-navigator"))
(unpin! evil-tmux-navigator)

(package! evil-replace-with-register)

(package! evil-exchange)

(package! evil-matchit)

(package! evil-textobj-line
  :recipe (:host github :repo "emacsorphanage/evil-textobj-line"))

(package! evil-snipe :disable t)

(unpin! org-roam)
(package! org-roam-ui)

(package! outshine
  :recipe (:host github :repo "alphapapa/outshine"))

(package! graphviz-dot-mode)

(package! ox-gfm)
