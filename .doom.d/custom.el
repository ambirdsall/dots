(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-alist
   '((("\\.vue\\'" . "Vue component")
      .
      ["template.vue" web-mode autoinsert-yas-expand])))
 '(auto-insert-directory "~/autoinsert-templates/")
 '(auto-insert-query nil t)
 '(custom-safe-themes
   '("7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
     (outshine-mode . 1)
     (outshine-mode . t)))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (defvaralias))))
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
