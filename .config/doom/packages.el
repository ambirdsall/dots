;; -*- no-byte-compile: t; -*-

(package! kkp)

(package! clipetty)

(package! evil-replace-with-register)

(package! evil-exchange)

(package! evil-matchit)

(package! evil-snipe :disable t)

(package! evil-textobj-line
  :recipe (:host github :repo "emacsorphanage/evil-textobj-line"))

(package! command-log-mode)

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

(package! typst-mode)
(package! ox-typst)

(package! flycheck-janet
  :recipe '(:host github :repo "sogaiu/flycheck-janet" :files ("*.el")))

(package! kdl-mode)

(package! gptel)

(package! graphviz-dot-mode)

(package! ox-gfm)

(package! olivetti)

(unpin! org-roam)
(package! org-roam-ui)

(package! outshine
  :recipe (:host github :repo "alphapapa/outshine"))
