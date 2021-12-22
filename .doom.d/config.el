;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.

;; * whoami
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Birdsall"
      user-mail-address "ambirdsall@gmail.com")

;; GUI emacs? sure, be cute.
;; In the terminal? I said what I said.
(unless (display-graphic-p) (setq confirm-kill-emacs nil))

;; * i got this footgun for self defuns
(after! projectile
  (defun yank-buffer-filename-relative-to-project ()
    "Copy the current buffer's path, relative to the project root, to the kill ring."
    (interactive)
    (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
        (message (kill-new (f-relative filename (projectile-acquire-root))))
      (error "Couldn't find filename in current buffer"))))

(defmacro on-string-or-region (fn)
  "Given a string-manipulation function, defines an interactive command which will apply that
function to either a string argument or to selected text, depending on context."
  `(lambda (string &optional from to)
     (interactive
      (if (use-region-p)
          (list nil (region-beginning) (region-end))
        (let ((bds (bounds-of-thing-at-point 'paragraph)))
          (list nil (car bds) (cdr bds)))))

     (let* ((work-on-string? (if string t nil))
            (input-str (if work-on-string?
                           string
                         (buffer-substring-no-properties from to)))
            (output-str (funcall ,fn input-str)))

       (if work-on-string?
           output-str
         (save-excursion
           (delete-region from to)
           (goto-char from)
           (insert output-str))))))

(defmacro def-text-operator (name fn)
  "Create a new interactive command bound to NAME using some
string manipulation function FN. It will work given a string
argument programmatically or by operating on selected text when
used interactively."
  `(fset ,name (on-string-or-region ,fn)))

(def-text-operator 'kebab-case #'s-dashed-words)
(def-text-operator 'pascal-case #'s-upper-camel-case)
(def-text-operator 'camel-case #'s-lower-camel-case)
(def-text-operator 'snake-case #'s-snake-case)
(def-text-operator 'screaming-snake-case #'(lambda (str) (s-upcase (s-snake-case str))))
(def-text-operator 'lower-words-case #'(lambda (str) (s-join " " (-map #'s-downcase (s-split-words str)))))

;; * make it pretty
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
(setq doom-font "Fira Code")
;; ;; why doesn't the sizing work?!?!?!?!?!?!?!?
(setq doom-variable-pitch-font (if IS-MAC "Baskerville-18" "LibreBaskerville"))

;; * Theme this bad boy
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-rouge)
(defun amb/toggle-themes ()
  (interactive)
  (cond ((eq doom-theme 'doom-rouge) (load-theme 'tsdh-light))
        (t (load-theme 'doom-rouge))))

;; TODO: amb/random-theme

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; * Keybindings
;; ** lead me to space
(map!
 :leader
 :desc "prior buffer" "=" #'evil-switch-to-windows-last-buffer
 "fY" #'yank-buffer-filename-relative-to-project
 "hT" #'amb/toggle-themes
 "Nr" #'narrow-to-region
 "Nf" #'narrow-to-defun
 "Np" #'narrow-to-page
 "Ns" #'org-toggle-narrow-to-subtree
 "Nw" #'widen
 :desc "jump to first non-blank" "of" #'evil-first-non-blank
 :desc "new frame" "oF" #'make-frame
 :desc "Open project TODOs.org file" "po" #'amb/goto-project-todos
 "W" #'subword-mode)

;; ** evil and global bindings
(map!
 "C-;" #'evil-avy-goto-char-timer
 :ni "C-)" #'sp-forward-slurp-sexp
 :ni "C-(" #'sp-backward-slurp-sexp
 :n "M-/" #'+default/search-buffer
 (:when (not (display-graphic-p)) :map (evil-insert-state-map evil-motion-state-map) "C-z" #'suspend-frame))

;; * evil config
(setq! evil-ex-search-persistent-highlight nil
       +evil-want-o/O-to-continue-comments nil)

(use-package! evil-replace-with-register
  :init
  (setq evil-replace-with-register-key (kbd "gr"))
  :config (evil-replace-with-register-install))
(use-package! evil-exchange
  :config (evil-exchange-install))
(use-package! evil-tmux-navigator
  :config (evil-tmux-navigator-bind-keys))
(use-package! evil-matchit
  :config (global-evil-matchit-mode 1))



;; * languages
(use-package! fennel-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))
(use-package! graphql-mode)
(setq typescript-indent-level 2)

;; ** elixir
(after! alchemist-mode
  (map! (:when (featurep! :lang elixir)    ; local conditional
        (:map alchemist-mode-map
         :n
         "C-j" #'tmux-navigate-down
         "C-k" #'tmux-navigate-up
         :localleader
         "tt" #'exunit-toggle-file-and-test
         "tT" #'exunit-toggle-file-and-test-other-window))))

;; ** web-mode
(setq! web-mode-markup-indent-offset 2
       web-mode-css-indent-offset 2
       web-mode-code-indent-offset 2)

(setq! web-mode-engines-alist
      '(("angular" . "\\.html")
        ("vue" . "\\.vue")
        ("phoenix" . "\\.html.eex")
        ("erb" . "\\.html\\.erb")))

;; ** lsp
(use-package! lsp-tailwindcss)
;; * computer-wide settings
(setq! mac-command-modifier 'meta
       mac-option-modifier 'meta
       ns-function-modifier 'super)
(setq! projectile-project-search-path '("~/c/"))
(setq! fill-column 100)
(global-visual-line-mode -1)

;; * org-mode config
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; share a global todo.org file freely between projects with ~ln~, without breaking projectile workflows
;; by opening different filepaths to the same inode in different buffers
(setq find-file-existing-other-name nil)

(after! projectile
  (defun amb/goto-project-todos ()
    (interactive)
    (find-file (concat (projectile-project-root) "todo.org"))))

; TODO: copy the contents of existing notes somewhere, set this to ~/Dropbox/org, and laugh all the
; way to the b̶a̶n̶k̶ knowledge graph
(setq org-roam-directory "~/roam/")

; make
(setq! org-hierarchical-todo-statistics nil)

;; ** TODO get shit done
(setq! org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "BLOCKED(b)" "SOMEDAY(s)" "PROJ(p)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c)")))
;; TODO verify whether explicitly setting agenda files prevents automatic
;; detection of new files in ~/notes/*.org
(setq! org-agenda-files '("~/Dropbox/org/todo.org"
                          "~/c/monorail/todo.org"
                          "~/Dropbox/org/notes.org"
                          "/Users/alex.birdsall/Dropbox/org/car.org"
                          "/Users/alex.birdsall/Dropbox/org/doom.org"
                          "/Users/alex.birdsall/Dropbox/org/food.org"
                          "/Users/alex.birdsall/Dropbox/org/indiegogo.org"
                          "/Users/alex.birdsall/Dropbox/org/linux.org"
                          "/Users/alex.birdsall/Dropbox/org/nba.org"
                          "/Users/alex.birdsall/Dropbox/org/house.org"))
(setq! org-log-into-drawer t)
(setq! org-refile-use-outline-path 'full-file-path)

;; This pair lets you open the same hardlinked {multiple,project,repos}/todo.org inode in multiple
;; project-specific buffers, each respecting the local filename and context (important for
;; maintaining the correct context for e.g. projectile functions)
(setq! find-file-existing-other-name nil
       find-file-visit-truename nil)

;; ** agenda viewing
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

;; ** keybinding fixes
(map! :after org
 :map 'org-mode-map
      "<tab>" 'org-cycle)

;; ** ✨ org everywhere ✨
(use-package! outshine
  :after org
  :config
  (add-hook 'prog-mode-hook 'outshine-mode)
  ;; (defvar outline-minor-mode-prefix "\M-#")
  )

;; ** make it pretty 💅
;; (use-package! mixed-pitch
;;   :hook (org-mode . #'mixed-pitch-mode)
;;   :config
;;   (setq mixed-pitch-set-heigth t)
;;   (set-face-attribute 'variable-pitch nil :height 180)
;;   (setq mixed-pitch-variable-pitch-cursor nil))
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
;; (setq mixed-pitch-variable-pitch-cursor nil)
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(setq!
 org-hide-emphasis-markers t
 org-agenda-filter-preset '("-quotidian"))

;;** 📉_(ツ)_📈
(use-package! graphviz-dot-mode
  :after org)

;; TODO: figure out doom's org exporter API
;; (after! org
;;   '(require 'ox-gfm nil t))

;; * git
;; ** nice git conflic resolution hydra
;; all thanks and apologies to https://github.com/alphapapa/unpackaged.el
(use-package! smerge-mode
  :after (hydra magit)
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

;; ** make magit play nicely with window configurations
(after! magit
  ;; strictly speaking unnecessary (it's the default)
  ;; (add-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration))

;; * code compass
(use-package! code-compass :defer t)

;; * private and/or work-specific config
(let ((private-config (concat doom-private-dir "local.el")))
  (and (file-exists-p private-config) (load private-config)))
