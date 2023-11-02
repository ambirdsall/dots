;; -*- no-byte-compile: t; -*-

(package! benchmark-init)

(package! emojify)

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

(package! fennel-mode)

(package! graphql-mode)

(package! yuck-mode)

(package! apheleia)

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! yaml-pro)

(unpin! org-roam)
(package! org-roam-ui)

(package! outshine
  :recipe (:host github :repo "alphapapa/outshine"))

(package! graphviz-dot-mode)

(package! ox-gfm)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! gptel)
