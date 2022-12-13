;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alex Birdsall"
      user-mail-address "ambirdsall@gmail.com")

;; GUI emacs? sure, be cute.
;; In the terminal? I said what I said.
(unless (display-graphic-p) (setq confirm-kill-emacs nil))

(map! :leader
      :desc "open doom config" "F" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "open doom config" "fP" (cmd! (find-file (expand-file-name "config.org" doom-private-dir))))

(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))
(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)

(setq! mac-command-modifier 'meta
       mac-option-modifier 'meta
       ns-function-modifier 'super)

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ne "f" #'find-file
        :desc "Recent files" :ne "r" #'consult-recent-file
        :desc "Config dir" :ne "C" #'doom/open-private-config
        :desc "Open config.org" :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
        :desc "Open dotfile" :ne "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)" :ne "n" #'org-roam-node-find
        :desc "Switch buffer" :ne "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ne "B" #'consult-buffer
        :desc "IBuffer" :ne "i" #'ibuffer
        :desc "Previous buffer" :ne "p" #'previous-buffer
        :desc "Set theme" :ne "t" #'consult-theme
        :desc "Quit" :ne "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ne "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(setq doom-font "Fira Code")
;; ;; why doesn't the sizing work?!?!?!?!?!?!?!?
(setq doom-variable-pitch-font (if IS-MAC "Baskerville-18" "Carlito"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq amb/doom-dark-theme 'doom-1337
      amb/doom-light-theme 'doom-opera-light)

;; TODO: amb/random-theme
;; this uses
(defun amb/toggle-themes ()
  "Cycle through a set of predefined themes according to whatever unholy logic is currently residing in its inner `cond' form."
  (interactive)
  (cond ((eq doom-theme amb/doom-dark-theme) (load-theme amb/doom-light-theme))
        (t (load-theme amb/doom-dark-theme))))

(setq display-line-numbers-type 'relative)

(setq! fill-column 90)
(global-visual-line-mode -1)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(defmacro on-string-or-region (fn)
  "Given a string-manipulation function FN, defines an interactive command which will apply that
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

;; TODO use defalias instead of fset so docstrings can be set
(defmacro def-text-transform (name fn)
  "Create a new interactive command bound to NAME using some
string manipulation function FN. It will work given a string
argument programmatically or by operating on selected text when
used interactively."
  `(fset ,name (on-string-or-region ,fn)))

(def-text-transform 'kebab-case #'s-dashed-words)
(def-text-transform 'pascal-case #'s-upper-camel-case)
(def-text-transform 'camel-case #'s-lower-camel-case)
(def-text-transform 'snake-case #'s-snake-case)
(def-text-transform 'screaming-snake-case #'(lambda (str) (s-upcase (s-snake-case str))))
(def-text-transform 'lower-words-case #'(lambda (str) (s-join " " (-map #'s-downcase (s-split-words str)))))

(map!
 :leader
 :desc "prior buffer" "=" #'evil-switch-to-windows-last-buffer
 "Nr" #'narrow-to-region
 "Nf" #'narrow-to-defun
 "Np" #'narrow-to-page
 "Ns" #'org-toggle-narrow-to-subtree
 "Nw" #'widen
 :desc "jump to first non-blank" "of" #'evil-first-non-blank
 :desc "new frame" "oF" #'make-frame
 :desc "Open project TODOs.org file" "po" #'amb/goto-project-todos
 "tt" #'amb/toggle-themes
 "W" #'subword-mode)

(map!
 "C-;" #'evil-avy-goto-char-timer
 :ni "C-)" #'sp-forward-slurp-sexp
 :ni "C-(" #'sp-backward-slurp-sexp
 :n "M-/" #'+default/search-buffer
 (:when (not (display-graphic-p)) :map (evil-insert-state-map evil-motion-state-map) "C-z" #'suspend-frame))

(after! projectile
  (defun yank-buffer-filename-relative-to-project ()
    "Copy the current buffer's path, relative to the project root, to the kill ring."
    (interactive)
    (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
        (message (kill-new (f-relative filename (projectile-acquire-root))))
      (error "Couldn't find filename in current buffer"))))

(map! :leader "fY" #'yank-buffer-filename-relative-to-project)

(setq! doom-scratch-initial-major-mode 'org-mode)

(after! persp-mode (setq! persp-emacsclient-init-frame-behaviour-override -1))

(after! projectile
  (defmacro file-jumper-for-project (project-root)
    "Defines an anonymous interactive function for picking an arbitrary file from the given PROJECT-ROOT.

Conveniently, by explicitly providing the project root, you can use the conveniently
flattened file hierarchy generated by `projectile-project-files' regardless of whether
projectile would recognize your root directory as a project."
    `(cmd! (find-file (string-join
                       (list
                        ,project-root
                        (projectile-completing-read "Find file: " (projectile-project-files ,project-root)))
                       "/"))))

  (map! :leader
        :desc "Browse dotfiles" "f." (cmd! (find-file
                                         (completing-read "Open dotfile: "
                                                          (split-string (shell-command-to-string "dots ls-files ~") "\n"))))
        :prefix ("fj" . "Jump into specific projects")
        :desc "Browse ~/.config/" :ne "c" (file-jumper-for-project "~/.config")
        :desc "Browse ~/bin/" :ne "b" (file-jumper-for-project "~/bin")))

(use-package! fennel-mode
  :config (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

(use-package! graphql-mode)

(after! alchemist-mode
  (map! (:when (modulep! :lang elixir)    ; local conditional
        (:map alchemist-mode-map
         :n
         "C-j" #'tmux-navigate-down
         "C-k" #'tmux-navigate-up
         :localleader
         "tt" #'exunit-toggle-file-and-test
         "tT" #'exunit-toggle-file-and-test-other-window))))

(use-package! apheleia
  :hook ((tsx-mode . apheleia-mode)
         (typescript-mode . apheleia-mode)
         (typescript-tsx-mode . apheleia-mode)
         (js-mode . apheleia-mode)
         (json-mode . apheleia-mode)
         (css-mode . apheleia-mode)
         (scss-mode . apheleia-mode))
  :defer t
  :config
  (push '(tsx-mode . prettier) apheleia-mode-alist)
  (push '(scss-mode . prettier) apheleia-mode-alist)
  (push '(css-mode . prettier) apheleia-mode-alist))

(setq! web-mode-markup-indent-offset 2
       web-mode-css-indent-offset 2
       web-mode-code-indent-offset 2)

(setq! web-mode-engines-alist
      '(("angular" . "\\.html")
        ("vue" . "\\.vue")
        ("phoenix" . "\\.html\\.eex")
        ("erb" . "\\.html\\.erb")))

(use-package! lsp-tailwindcss
  :after lsp)

(after! magit
  ;; strictly speaking unnecessary (it's the default)
  ;; (add-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)

  (defun just-use-a-dash-instead-sheesh (_nope &rest _dontcare)
    (interactive)
    (self-insert-command 1 ?-))
  
  (advice-add 'magit-whitespace-disallowed :around #'just-use-a-dash-instead-sheesh)

  (setq! magit-section-initial-visibility-alist '((stashes . show) (commits . show))))

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

;; strictly speaking unnecessary (it's the default)
;; (add-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq magit-bury-buffer-function #'magit-restore-window-configuration)

(defun just-use-a-dash-instead-sheesh (_nope &rest _dontcare)
  (interactive)
  (self-insert-command 1 ?-))

(advice-add 'magit-whitespace-disallowed :around #'just-use-a-dash-instead-sheesh)

(setq! magit-section-initial-visibility-alist '((stashes . show) (commits . show)))

(setq! projectile-project-search-path '("~/c/"))

(use-package! code-compass :defer t
              :commands (c/show-hotspots-sync
                         c/show-hotspot-snapshot-sync
                         c/show-code-churn-sync
                         c/show-coupling-graph-sync
                         c/show-code-communication-sync
                         c/show-knowledge-graph-sync
                         c/show-code-age-sync
                         c/show-fragmentation-sync
                         c/show-hotspot-cluster-sync)
              :config
              (setq c/exclude-directories (list "node_modules" "bower_components" "vendor" "tmp" "images"))
              (if IS-MAC (setq c/preferred-browser "open")))

(use-package! evil-tmux-navigator
  :config (evil-tmux-navigator-bind-keys))

(use-package! evil-replace-with-register
  :init (setq evil-replace-with-register-key (kbd "gr"))
  :config (evil-replace-with-register-install))

(use-package! evil-exchange
  :config (evil-exchange-install))

(use-package! evil-matchit
  :config (global-evil-matchit-mode 1))

(use-package! evil-textobj-line
  :after evil)

(setq! evil-ex-search-persistent-highlight nil
       +evil-want-o/O-to-continue-comments nil)

(setq org-directory "~/Dropbox/org/")

(setq! org-log-into-drawer t
       org-hierarchical-todo-statistics nil
       org-refile-use-outline-path 'full-file-path
       org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "BLOCKED(b)" "SOMEDAY(s)" "PROJ(p)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c)")))

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

(add-hook! (org-mode) (org-appear-mode 1))

(setq org-roam-directory "~/Dropbox/roam/")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq! find-file-existing-other-name nil
       find-file-visit-truename nil)

(after! projectile
  (defun amb/goto-project-todos ()
    (interactive)
    ;; TODO dynamically create one if missing? This system can be improved further.
    (find-file (concat (projectile-project-root) "todo.org"))))

;; TODO verify whether explicitly setting agenda files prevents automatic
;; detection of new files in ~/notes/*.org
(setq! org-agenda-files '("~/Dropbox/org/todo.org"
                          "~/Dropbox/org/notes.org"
                          "~/Dropbox/org/car.org"
                          "~/Dropbox/org/doom.org"
                          "~/Dropbox/org/food.org"
                          "~/Dropbox/org/igg.org"
                          "~/Dropbox/org/linux.org"
                          "~/Dropbox/org/nba.org"
                          "~/Dropbox/org/house.org"))

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

(map! :after org
 :map 'org-mode-map
      "<tab>" 'org-cycle)

(use-package! outshine
  :after org
  :config
  (add-hook 'prog-mode-hook 'outshine-mode))

(use-package! graphviz-dot-mode
  :after org)

;; TODO: figure out doom's org exporter API
;; (after! org
;;   '(require 'ox-gfm nil t))
(use-package! ox-gfm
  :after org)

(defvar amb/computer-specific-config (expand-file-name "local.el" doom-private-dir)
  "A file for computer-specific config, hidden from git; for
example, configuration for a work computer and its (possibly
private) product projects.")

(let ((amb/computer-specific-config (concat doom-private-dir "local.el")))
  (and (file-exists-p amb/computer-specific-config) (load amb/computer-specific-config)))

(map! :leader
      :desc "open computer-specific doom config" "fL" (cmd! (find-file amb/computer-specific-config)))

;; (after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
;;           projectile-project-root-files-bottom-up)))