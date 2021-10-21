(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#E5E9F0" "#99324B" "#4F894C" "#9A7500" "#3B6EA8" "#97365B" "#398EAC" "#3B4252"])
 '(auto-insert 'other)
 '(auto-insert-alist
   '((("\\.vue\\'" . "Vue component")
      .
      ["template.vue" web-mode autoinsert-yas-expand])))
 '(auto-insert-directory "~/autoinsert-templates/")
 '(auto-insert-query nil)
 '(c/exclude-directories
   '("node_modules" "bower_components" "vendor" "tmp" "images"))
 '(c/preferred-browser "open")
 '(custom-safe-themes
   '("1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(diary-file "~/notes/diary-google")
 '(fci-rule-color "#AEBACF")
 '(fill-column 100)
 '(jdee-db-active-breakpoint-face-colors (cons "#F0F4FC" "#5d86b6"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#F0F4FC" "#4F894C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#F0F4FC" "#B8C5DB"))
 '(objed-cursor-color "#99324B")
 '(org-agenda-files
   '("~/c/monorail/todo.org" "~/Dropbox/org/todo.org" "/Users/alex.birdsall/Dropbox/org/car.org" "/Users/alex.birdsall/Dropbox/org/doom.org" "/Users/alex.birdsall/Dropbox/org/food.org" "/Users/alex.birdsall/Dropbox/org/linux.org" "/Users/alex.birdsall/Dropbox/org/nba.org"))
 '(org-agenda-include-diary t)
 '(package-selected-packages '(graphql-mode yaml-mode mixed-pitch))
 '(pdf-view-midnight-colors (cons "#3B4252" "#E5E9F0"))
 '(rustic-ansi-faces
   ["#E5E9F0" "#99324B" "#4F894C" "#9A7500" "#3B6EA8" "#97365B" "#398EAC" "#3B4252"])
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (auto-save-default)))
 '(vc-annotate-background "#E5E9F0")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4F894C")
    (cons 40 "#688232")
    (cons 60 "#817b19")
    (cons 80 "#9A7500")
    (cons 100 "#a0640c")
    (cons 120 "#a65419")
    (cons 140 "#AC4426")
    (cons 160 "#a53f37")
    (cons 180 "#9e3a49")
    (cons 200 "#97365B")
    (cons 220 "#973455")
    (cons 240 "#983350")
    (cons 260 "#99324B")
    (cons 280 "#a0566f")
    (cons 300 "#a87b93")
    (cons 320 "#b0a0b6")
    (cons 340 "#AEBACF")
    (cons 360 "#AEBACF")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((elixir-mode-hook) (lsp-mode)))
 '(web-mode-markup-indent-offset 2 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(outline-1 ((t (:weight extra-bold :height 1.25))))
 '(outline-2 ((t (:weight bold :height 1.15))))
 '(outline-3 ((t (:weight bold :height 1.12))))
 '(outline-4 ((t (:weight semi-bold :height 1.09))))
 '(outline-5 ((t (:weight semi-bold :height 1.06))))
 '(outline-6 ((t (:weight semi-bold :height 1.03))))
 '(outline-8 ((t (:weight semi-bold))))
 '(outline-9 ((t (:weight semi-bold)))))
(put 'narrow-to-region 'disabled nil)
