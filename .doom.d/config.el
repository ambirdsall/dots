;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Birdsall"
      user-mail-address "ambirdsall@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font "FiraCode")
;; ;; why doesn't the sizing work?!?!?!?!?!?!?!?
(setq doom-variable-pitch-font (if IS-MAC "Baskerville-18" "LibreBaskerville"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; * Keybindings
;; ** lead me to space
(map!
 :leader
 "W" #'subword-mode
 :desc "jump to first non-blank" "of" #'evil-first-non-blank
 :desc "new frame" "oF" #'make-frame
 :desc "Open project TODOs.org file" "po" (cmd! (find-file (concat (projectile-project-root) "todo.org"))))

;; ** evil and global bindings
(map!
 (:when (not (display-graphic-p)) :map (evil-insert-state-map evil-motion-state-map) "C-z" #'suspend-frame)
 "C-;" #'evil-avy-goto-char-timer
 :ni "C-)" #'sp-forward-slurp-sexp
 :ni "C-(" #'sp-backward-slurp-sexp)

;; * evil config
(setq! evil-disable-insert-state-bindings t
       evil-ex-search-persistent-highlight nil)

(use-package! evil-replace-with-register
  :init
  (setq evil-replace-with-register-key (kbd "gr"))
  :config
  (evil-replace-with-register-install))
(use-package! evil-exchange
  :config
  (evil-exchange-install))
(use-package! evil-tmux-navigator
  :config
  (evil-tmux-navigator-bind-keys))
(after! evil-snipe (evil-snipe-mode -1))

;; * languages
(use-package! fennel-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))
(use-package! graphql-mode)

;; * computer-wide settings
(setq! mac-command-modifier 'meta
       mac-option-modifier 'super
       ns-function-modifier 'hyper)
(setq! projectile-project-search-path '("~/c/"))
(setq! fill-column 100)

;; * org-mode config
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
;; TODO: add a second, personal org-roam directory
(setq org-roam-directory "~/roam/")

;; ** TODO get shit done
(setq! org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "BLOCKED(b)" "SOMEDAY(s)" "PROJ(p)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c)")))
;; TODO verify whether explicitly setting agenda files prevents automatic
;; detection of new files in ~/notes/*.org
(setq! org-agenda-files '("~/Dropbox/org/todo.org" "~/c/monorail/todo.org" "/Users/alex.birdsall/Dropbox/org/car.org" "/Users/alex.birdsall/Dropbox/org/doom.org" "/Users/alex.birdsall/Dropbox/org/food.org" "/Users/alex.birdsall/Dropbox/org/indiegogo.org" "/Users/alex.birdsall/Dropbox/org/linux.org" "/Users/alex.birdsall/Dropbox/org/nba.org" "/Users/alex.birdsall/Dropbox/org/house.org"))
(setq! org-log-into-drawer t)
(setq! org-refile-use-outline-path 'full-file-path)

(defun org-my-auto-exclude-fn (tag)
  (if (cond
       ;; TODO show only the next 2
       ((string= tag "quotidian")
        t)
       ;; only see work things between 8am and 7pm
       ((string= tag "work")
        (let ((hr (nth 2 (decode-time))))
          (or (< hr 8) (> hr 19)))))
      (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-fn)


;; ** ✨ org everywhere ✨
(use-package! outshine
  :init
  (defvar outline-minor-mode-prefix "\M-#"))
(add-hook! 'prog-mode-hook #'outshine-mode)

;; ** make it pretty 💅
(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-heigth t)
  (set-face-attribute 'variable-pitch nil :height 180)
  (setq mixed-pitch-variable-pitch-cursor nil)
  (add-hook! 'org-mode-hook #'mixed-pitch-mode))

(setq!
 org-hide-emphasis-markers t
 org-agenda-filter-preset '("-quotidian"))

;;** 📉_(ツ)_📈
(use-package! graphviz-dot-mode)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; * nice git conflic resolution hydra
;; all thanks and apologies to https://github.com/alphapapa/unpackaged.el
(use-package! smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;; * auto-expanding snippets for new file templates
  (unless auto-insert-mode (auto-insert-mode))

  (custom-set-variables
   '(auto-insert-query nil)
   '(auto-insert 'other)
   '(auto-insert-directory "~/autoinsert-templates/")
   '(auto-insert-alist '((("\\.vue\\'" . "Vue component") . ["template.vue" web-mode autoinsert-yas-expand]))))

;; * private and/or work-specific config
(let ((private-config (concat default-directory "local.el")))
  (and (file-exists-p private-config) (load private-config)))
