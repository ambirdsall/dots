;; -*- no-byte-compile: t; -*-

(package! kkp)

(package! clipetty)

(package! evil-replace-with-register)

(package! evil-exchange)

(package! evil-matchit)

(package! evil-snipe :disable t)

(package! evil-textobj-line
  :recipe (:host github :repo "emacsorphanage/evil-textobj-line"))

(package! ace-window)

(package! emojify)

(package! golden-ratio)

(package! code-compass
  :recipe (:host github :repo "ag91/code-compass" :files (:defaults "pages" "scripts")))

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! fennel-mode)

(package! graphql-mode)

(package! yuck-mode)

(package! yaml-pro)

(package! gptel)

(package! graphviz-dot-mode)

(package! ox-gfm)

(unpin! org-roam)
(package! org-roam-ui)

(package! outshine
  :recipe (:host github :repo "alphapapa/outshine"))
