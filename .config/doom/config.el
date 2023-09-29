;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alex Birdsall"
      user-mail-address "ambirdsall@gmail.com")

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

(setq fancy-splash-image (concat doom-private-dir "emacs.png"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(setq doom-font-increment 1
      doom-font (font-spec :family "Fira Code" :size (if IS-MAC 13 16) :style "Retina" :weight 'semi-bold)
      ;; doom-font (font-spec :family "Iosevka Fixed Slab" :size 16 :weight 'medium)
      doom-big-font (font-spec :family "Fira Code" :size (if IS-MAC 20 26))
      doom-variable-pitch-font (font-spec :family "Overpass" :size (if IS-MAC 15 20))
      doom-serif-font (font-spec :family "Iosevka Slab" :size (if IS-MAC 13 16))
      doom-unicode-font (font-spec :family "Iosevka" :size (if IS-MAC 13 16)))

(defvar mixed-pitch-modes '(org-mode markdown-mode gfm-mode Info-mode text-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(unless noninteractive
  (setq
   ;; amb/doom-dark-theme 'modus-vivendi
   amb/doom-dark-theme 'doom-badger
   amb/doom-light-theme 'modus-operandi)

  (defun amb/toggle-themes ()
    "Cycle through a set of predefined themes according to whatever unholy logic is currently residing in its inner `cond' form."
    (interactive)
    (cond ((eq doom-theme amb/doom-dark-theme) (load-theme amb/doom-light-theme))
          (t (load-theme amb/doom-dark-theme))))

  (map! :leader
        "tt" #'amb/toggle-themes)

  (load-theme amb/doom-dark-theme t))

(unless noninteractive
  (require-theme 'modus-themes)

  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-syntax (list 'alt-syntax 'yellow-comments)
        modus-themes-vivendi-color-overrides
        '((bg-main . "#0d0b11")
          (fg-main . "#ffeeee")
          (bg-hl-line . "#29272f"))
        modus-themes-operandi-color-overrides
        '((bg-hl-line . "#eeeeee"))))

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

(custom-set-faces!
  '(+workspace-tab-face :inherit default :family "Overpass" :height 135)
  '(+workspace-tab-selected-face :inherit (highlight +workspace-tab-face)))

(tab-bar-history-mode)

(after! persp-mode
  (defun workspaces-formatted ()
    ;; fancy version as in screenshot
    (+doom-dashboard--center (frame-width)
                             (let ((names (or persp-names-cache nil))
                                   (current-name (safe-persp-name (get-current-persp))))
                               (mapconcat
                                #'identity
                                (cl-loop for name in names
                                         for i to (length names)
                                         collect
                                         (concat (propertize (format " %d" (1+ i)) 'face
                                                             `(:inherit ,(if (equal current-name name)
                                                                             '+workspace-tab-selected-face
                                                                           '+workspace-tab-face)
                                                               :weight bold))
                                                 (propertize (format " %s " name) 'face
                                                             (if (equal current-name name)
                                                                 '+workspace-tab-selected-face
                                                               '+workspace-tab-face))))
                                " "))))
  (defun amb/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right amb/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))

(defmacro cmds--on-string-or-region (fn)
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
  `(fset ,name (cmds--on-string-or-region ,fn)))

(def-text-transform 'kebab-case #'s-dashed-words)
(def-text-transform 'pascal-case #'s-upper-camel-case)
(def-text-transform 'camel-case #'s-lower-camel-case)
(def-text-transform 'snake-case #'s-snake-case)
(def-text-transform 'screaming-snake-case #'(lambda (str) (s-upcase (s-snake-case str))))
(def-text-transform 'lower-words-case #'(lambda (str) (s-join " " (-map #'s-downcase (s-split-words str)))))

(defun decrement-number-at-point ()
  "Decrement the number at point by 1."
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (when (looking-at "[0-9]+")
      (let ((num (string-to-number (match-string 0))))
        (replace-match (number-to-string (1- num)))))))

(defun increment-number-at-point ()
  "Increment the number at point by 1."
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (when (looking-at "[0-9]+")
      (let ((num (string-to-number (match-string 0))))
        (replace-match (number-to-string (1+ num)))))))

(map! :leader
      :desc "decrement at point" "nj" #'decrement-number-at-point
      :desc "increment at point" "nk" #'increment-number-at-point)

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
        :desc "Take me $HOME, country roads" "f~" (cmd! (+vertico/find-file-in "~/"))
        :prefix ("fj" . "Jump into specific projects")
        :desc "Browse ~/.config/" :ne "c" (file-jumper-for-project "~/.config/")
        :desc "Browse ~/bin/" :ne "b" (file-jumper-for-project "~/bin/")))

(setq standard-indent 2)

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

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

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

(setq! geiser-active-implementations '(guile))

(defun insert-guile-shebang ()
  (interactive)
  (save-excursion
   (beginning-of-buffer)
   (insert "#!/usr/local/bin/guile \\
-e main -s
!#

")))

(use-package! yaml-pro
  :config (add-to-list 'auto-mode-alist '("\\.ya?ml'" . yaml-pro-ts-mode)))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion-by-word)
              ("TAB" . 'copilot-accept-completion-by-word)
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)))

(use-package! gptel)

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

(defun amb/magit-status-with-dotfiles-fallback ()
  (interactive)
  (if (magit-gitdir)
      (magit-status)
    (magit-status "~/")))

(map! :after magit :leader "g g" #'amb/magit-status-with-dotfiles-fallback)

;; from https://github.com/magit/magit/issues/460

(defun amb/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory."
  (let ((here (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= here home)
      (let ((gitdir (expand-file-name "~/.dots/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'amb/magit-process-environment)

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
       +evil-want-o/O-to-continue-comments nil
       select-enable-clipboard nil)

(map! :after consult "M-i" #'consult-imenu)

(let ((dir "~/Dropbox/org/"))
  (and (file-exists-p dir)
       (setq org-directory dir)))

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

(setq! org-agenda-files '("~/Dropbox/org/"
                          "~/Dropbox/roam/daily/"))

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

(setq confirm-kill-emacs nil)
