;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; commentary: what the fuck

(defvar amb/computer-specific-config (expand-file-name "local-config.el" doom-private-dir)
  "A file for computer-specific config and overrides, hidden from git; for example,
configuration for a work computer and its (possibly private) product projects.")

(defvar amb/computer-specific-toggles (expand-file-name "local-toggles.el" doom-private-dir)
  "A file for {en,dis}abling configuration and features on a computer-specific basis,
hidden from git; for example, configuration for proprietary
features enabled for a work computer by a company account.")

(defvar amb/enable-workspace-tabs nil
  "Do I really want to show tabs of the workspace names?")

(defvar amb/enable-copilot nil
  "Is my company paying for, and actively encouraging me to use, github copilot?")

(if (file-exists-p amb/computer-specific-toggles)
      (load amb/computer-specific-toggles))

(setq! select-enable-clipboard nil)

(map! "C-M-y" #'clipboard-yank)

(evil-define-operator evil-yank-to-clipboard (beg end type register yank-handler)
  "Save the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register)))
        (select-enable-clipboard t)
        (select-enable-primary t))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (evil-yank-rectangle beg end register yank-handler))
     ((memq type '(line screen-line))
      (evil-yank-lines beg end register yank-handler))
     (t
      (evil-yank-characters beg end register yank-handler)
      (goto-char beg)))))

(map! :map evil-normal-state-map "Y" #'evil-yank-to-clipboard)
(map! :map evil-motion-state-map "Y" #'evil-yank-to-clipboard)

(defun copy-to-clipboard (string)
  "Copies `STRING' to the system clipboard and the kill ring. When called interactively,
the active region will be used."
  (interactive
   (when (region-active-p)
     (list (buffer-substring-no-properties (region-beginning) (region-end)))))
  (let ((select-enable-clipboard t)
        (select-enable-primary t))
    (kill-new string)))

(defun copy-unicode-char-to-clipboard ()
  "Interactively select a unicode character and copy it to the system clipboard."
  (interactive)
  (with-temp-buffer
    (call-interactively #'insert-char)
    (let ((char (buffer-string)))
      (copy-to-clipboard char)
      (message "%s" (concat "Copied " char " to system clipboard")))))

(setq! mac-command-modifier 'meta
       mac-option-modifier 'meta
       ns-function-modifier 'super)

(use-package! kkp
  :if (not (display-graphic-p))
  :config
  (global-kkp-mode +1)
  (define-key! local-function-key-map
    [M-return] (kbd "M-RET")
    [M-tab] (kbd "M-TAB")
    [M-backspace] (kbd "M-DEL")
    (kbd "M-<backspace>") (kbd "M-DEL")
    [M-delete] (kbd "M-DEL")))

(add-hook! 'tty-setup-hook :depth -90
  (defun +tty-init-kkp-h ()
    (global-kkp-mode +1)
    (kkp-enable-in-terminal)))

(use-package! clipetty
  :if (not (display-graphic-p))
  :hook (after-init . global-clipetty-mode))

(setq remote-file-name-inhibit-locks t)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

(map! [remap dabbrev-expand] #'hippie-expand)

(setq company-global-modes '(not text-mode org-mode))

(use-package! evil-replace-with-register
  :init
  (setq evil-replace-with-register-key (kbd "gr"))
  :config (evil-replace-with-register-install))

(use-package! evil-exchange
  :config (evil-exchange-install))

(use-package! evil-matchit
  :config (global-evil-matchit-mode 1))

(setq! evil-ex-search-persistent-highlight nil
       +evil-want-o/O-to-continue-comments nil)

(map! :after consult "M-i" #'consult-imenu)

;; this macro was copied from someone who copied it from here: https://stackoverflow.com/a/22418983/4921402
(after! evil
  (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-" name))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key #',inner-name)
         (define-key evil-outer-text-objects-map ,key #',outer-name))))
  (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$")
  (define-and-bind-quoted-text-object "pipe" "|" "|" "|")
  (define-and-bind-quoted-text-object "slash" "/" "/" "/")
  (define-and-bind-quoted-text-object "space" " " " " " ")
  (define-and-bind-quoted-text-object "tilda" "~" "~" "~")
  (define-and-bind-quoted-text-object "asterisk" "*" "*" "*"))

(use-package! evil-textobj-line
  :after evil)

(setq! doom-snippets-enable-short-helpers 't)

(use-package! command-log-mode)

(defhydra amb/window-nav-hydra (:hint nil :exit nil)
  "
Navigate Windows (exit with RET, ESC, q, or C-g)
  ^Navigate^
  _h_ ←  _j_ ↓  _k_ ↑  _l_ →

  ^Rearrange^
  _H_ ←  _J_ ↓  _K_ ↑  _L_ →
  _x_ Close _s_/_v_ Split

  ^Repurpose^
  _._ Nearby file  _p_/_SPC_ Project file  _r_ Recent file
"
  ;; Navigation
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)

  ;; Move windows
  ("H" +evil/window-move-left)
  ("J" +evil/window-move-down)
  ("K" +evil/window-move-up)
  ("L" +evil/window-move-right)

  ;; Act on windows
  ("x" +workspace/close-window-or-workspace)
  ("." find-file)
  ("p" projectile-find-file)
  ("SPC" projectile-find-file)
  ("r" consult-recent-file)
  ("s" evil-window-split)
  ("v" evil-window-vsplit)

  ;; Exit hydra
  ("RET" nil :exit t)
  ("ESC" nil :exit t)
  ("q" nil :exit t)
  ("C-g" nil :exit t))

(map! :leader
      :desc "get movin'" "w." #'amb/window-nav-hydra/body
      :desc "Manage windows" "W" #'amb/window-nav-hydra/body)

(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always 't
        aw-dispatch-alist '((?x aw-delete-window "Delete Window")
                            (?m aw-swap-window "Swap Windows")
                            (?M aw-move-window "Move Window")
                            (?c aw-copy-window "Copy Window")
                            (?b aw-switch-buffer-in-window "Select Buffer")
                            (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
                            (?n aw-flip-window)
                            (?c aw-split-window-fair "Split Fair Window")
                            (?v aw-split-window-vert "Split Vert Window")
                            (?z aw-split-window-horz "Split Horz Window")
                            (?o delete-other-windows "Delete Other Windows")
                            (?? aw-show-dispatch-help)))
  (map! :leader "ww" #'ace-window)
  (custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "dark green"
    :weight bold :height 2.5 :box (:line-width 10 :color "dark green"))))

(after! dirvish
  (setq! dirvish-attributes '(collapse git-msg file-size)))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(after! projectile
  (defun yank-buffer-filename-relative-to-project ()
    "Copy the current buffer's path, relative to the project root, to the kill ring."
    (interactive)
    (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
        (message (kill-new (f-relative filename (projectile-acquire-root))))
      (error "Couldn't find filename in current buffer")))

  (map! :leader "fY" #'yank-buffer-filename-relative-to-project))

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

(setq! doom-scratch-initial-major-mode 'org-mode)

(after! persp-mode (setq! persp-emacsclient-init-frame-behaviour-override -1))

(setq confirm-kill-emacs nil)

(setq fancy-splash-image (concat doom-private-dir "emacs.png"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(setq doom-font-increment 1
      doom-font (font-spec :family "Victor Mono" :size (if IS-MAC 13 16) :weight 'semi-bold)
      ;; doom-font (font-spec :family "Iosevka Fixed Slab" :size 16 :weight 'medium)
      doom-big-font (font-spec :family "Victor Mono" :size (if IS-MAC 20 26))
      doom-variable-pitch-font (font-spec :family "Overpass" :size (if IS-MAC 15 20))
      doom-serif-font (font-spec :family "Iosevka Slab" :size (if IS-MAC 13 16))
      ;; doom-unicode-font (font-spec :family "Iosevka" :size (if IS-MAC 13 16)))
      doom-unicode-font (font-spec :family "Victor Mono" :size (if IS-MAC 13 16)))

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

(whitespace-mode 1)

(unless noninteractive
  (setq
   amb/doom-dark-theme 'doom-one
   amb/doom-light-theme (if (display-graphic-p) 'modus-operandi 'doom-one-light))

  (defun amb/toggle-themes ()
    "Cycle through a set of predefined themes according to whatever unholy logic is currently residing in its inner `cond' form."
    (interactive)
    (cond ((eq doom-theme amb/doom-dark-theme) (load-theme amb/doom-light-theme))
          (t (load-theme amb/doom-dark-theme))))

  (map! :leader
        "tt" #'amb/toggle-themes)

  (load-theme amb/doom-dark-theme t))

(setq display-line-numbers-type 't)

(after! evil
  (add-hook! '(evil-operator-state-entry-hook evil-visual-state-entry-hook)
    (setq display-line-numbers 'relative))

  (add-hook! '(evil-operator-state-exit-hook evil-visual-state-exit-hook)
    (setq display-line-numbers 't)))

(setq! fill-column 90)
(global-visual-line-mode -1)

(setq frame-title-format
      '(""
        (:eval
         (if-let ((workspace-name (safe-persp-name (get-current-persp))))
           (format "%s ⋮ " workspace-name)))
        (:eval
         (let ((project-name (projectile-project-name))
               (workspace-name (safe-persp-name (get-current-persp))))
           (unless (or (string= "-" project-name) (string= workspace-name project-name))
             (format (if (buffer-modified-p)  " ◉ %s / " " %s / ") project-name))))
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))))

(custom-set-faces!
  '(+workspace-tab-face :inherit default :family "Overpass" :height 135)
  '(+workspace-tab-selected-face :inherit (highlight +workspace-tab-face)))

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
                                         (concat (propertize (format " %d" i) 'face
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
  (advice-add #'+workspace-message :override #'ignore)

  ;; need to run this later for it to not break frame size for some reason
  (run-at-time
   nil
   nil
   (cmd!
    (when amb/enable-workspace-tabs
      (tab-bar-history-mode)
      (tab-bar-mode +1)))))

(map! :leader
      :desc "toggle tab bar" "tT" #'tab-bar-mode)

(use-package! golden-ratio
  :config
  (dolist
      (fn '(evil-window-left evil-window-down evil-window-up evil-window-right))
    (add-to-list 'golden-ratio-extra-commands fn))
  (map! :leader
        "wG" #'golden-ratio
        "wgg" #'golden-ratio-mode))

(defvar amb--more-current-window-original-sizes (make-hash-table :test 'eq)
  "A hash table storing the original sizes of windows so they can be restored by `amb/more-current-window'.")

(defun amb--more-current-window-save-original-size (win)
  "Ensure the window configuration relative to a window object `WIN' is stored.
Window sizes are stored in `amb--more-current-window-original-sizes'."
  (unless (gethash win amb--more-current-window-original-sizes)
    (puthash win (current-window-configuration) amb--more-current-window-original-sizes)))

(defun amb/more-current-window ()
  "Make the current window larger based on predefined breakpoints.
If the window occupies the entire frame, restore its original size."
  (interactive)
  (let* ((win (selected-window))
         (frame-width (frame-width))
         (window-width (window-total-width win)))
    (cond
     ;; If the window is maximized, restore its original size.
     ((and (window-full-width-p win) (window-full-height-p win))
      (message "there and, uh,")
      (when-let ((orig-size (gethash win amb--more-current-window-original-sizes)))
        (message "and back again")
        (set-window-configuration orig-size)
        (remhash win amb--more-current-window-original-sizes)))
     ;; If the width is less than 50% of the frame, increase it to 50%.
     ;; Yes, I compare against 48%, not 50%; I don't want to be stuck at 50% when I want *more*
     ((< (/ (float window-width) frame-width) 0.48)
      (message "fiddy")
      (amb--more-current-window-save-original-size win)
      (let ((target-width (floor (* 0.50 frame-width))))
        (adjust-window-trailing-edge win (- target-width window-width) t)))
     ;; If the width is less than 61% of the frame, use golden-ratio.
     ((< (/ (float window-width) frame-width) 0.61)
      (message "goldy")
      (amb--more-current-window-save-original-size win)
      (call-interactively #'golden-ratio))
     ;; If the width is less than 70%, enlarge the window.
     ((< (/ (float window-width) frame-width) 0.70)
      (message "biggie")
      (amb--more-current-window-save-original-size win)
      (doom/window-enlargen))
     ;; Otherwise, maximize the window.
     (t
      (message "all that and then some")
      (amb--more-current-window-save-original-size win)
      (doom/window-maximize-buffer)))))

;; Bind the command to the leader key.
(map! :leader
      :desc "more of current window"
      "wM" #'amb/more-current-window)

(setq standard-indent 2)

(setq! projectile-project-search-path '("~/c/"))

(setq! find-file-existing-other-name nil
       find-file-visit-truename nil)

(after! projectile
  (defun amb/goto-project-todos ()
    (interactive)
    ;; TODO dynamically create one if missing? This system can be improved further.
    (find-file (concat (projectile-project-root) "todo.org")))

  (map!
   :leader
   :desc "Open project TODOs.org file" "po" #'amb/goto-project-todos)

  (add-to-list 'projectile-globally-ignored-files "!todo.org")
  (add-to-list 'projectile-globally-ignored-files "!test.http"))

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
     (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(after! lsp-mode
  (defun amb/lsp-restart ()
    "The current lsp server? Turn it off and on again."
    (interactive)
    (lsp-disconnect)
    (lsp!))

  (defun amb/lsp-execute-code-action-if-you-are-into-that ()
    "Like lsp-execute-code-action, but in cases where there is only a single available
  action it asks for confirmation rather than unconditionally springing into action."
    ;; TODO implement the logic as described lol
    (call-interactively #'lsp-execute-code-action))

  (defun amb/lsp-dwim ()
    "If there are code actions at point, trigger that. If not, jump to definition."
    (interactive)
    (if (lsp-code-actions-at-point)
        (amb/lsp-execute-code-action-if-you-are-into-that)
      (call-interactively #'+lookup/definition)))

  (map!
   :gnvie "C-M-l" #'lsp-execute-code-action
   (:map lsp-mode-map :n "RET" #'amb/lsp-dwim)

  ;; but, like, it *is* a prefix key???
  ;; manually running this map! form after init works great; I suppose lsp module does some rebinding or some shit
  ;; (map! :leader :desc "restart server" "clR" #'amb/lsp-restart)
  ))

(after! lsp-ui
  (map!
   :leader :desc "show references" "cR" #'lsp-ui-peek-find-references))

(setq! web-mode-markup-indent-offset 2
       web-mode-css-indent-offset 2
       web-mode-code-indent-offset 2)

(setq! web-mode-engines-alist
       '(;("angular" . "\\.html")
         ("vue" . "\\.vue")
         ("phoenix" . "\\.html\\.eex")
         ("erb" . "\\.html\\.erb")))

(use-package! lsp-tailwindcss
  :after lsp)

(after! rbenv
  (global-rbenv-mode +1))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(ruby-mode . "ruby"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () '("bundle" "exec" "standardrb" "--lsp")))
                    :major-modes '(ruby-mode)
                    :server-id 'standardrb-lsp)))

(after! apheleia
  (add-to-list 'apheleia-mode-alist '(ruby-mode . ruby-standard)))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! (tree-sitter-langs consult)
  (defun fold-all-methods-in-class ()
    "Fold all methods within the current class in any Tree-sitter-enabled buffer."
    (interactive)
    (let* ((root-node (tsc-root-node tree-sitter-tree))
           (query (tsc-make-query
                   tree-sitter-language
                   "
                 (class_definition
                   body: (block
                     [
                       (function_definition
                         name: (identifier) @method-name
                         body: (block) @method-body)
                       (decorated_definition
                         definition: (function_definition
                           name: (identifier) @method-name
                           body: (block) @method-body))
                     ]
                   ))
                 "))
           (captures (tsc-query-captures query root-node #'tsc--buffer-input)))
      (dotimes (i (length captures))
        (let* ((capture (aref captures i))
               (capture-name (car capture))
               (capture-node (cdr capture)))
          (when (string= capture-name "method-name")
            (save-excursion
              (goto-char (tsc-node-start-position capture-node))
              (+fold/close)))))))

  (defun amb/tree-sitter-list-functions ()
    "Extract function and method definitions with class and function nesting using Tree-sitter."
    (let* ((root-node (tsc-root-node tree-sitter-tree))
           ;; Tree-sitter query to capture function definitions
           (query (tsc-make-query
                   tree-sitter-language
                   "
                 (function_definition
                   name: (identifier) @func-name)
                 "))
           (captures (tsc-query-captures query root-node #'tsc--buffer-input))
           (current-nest '()) ;; Stack to track class/function nesting
           (functions '()))   ;; Store the functions to return for Imenu
      (dotimes (i (length captures))
        (let* ((capture (aref captures i))
               (func-node (cdr capture))
               (func-name (tsc-node-text func-node))
               ;; Determine function's starting position
               (pos (tsc-node-start-position func-node)))
          ;; Update current nesting context by checking parent nodes
          (setq current-nest (amb/get-nesting-context func-node))
          ;; Create the Imenu entry
          (push (cons (string-join (append current-nest (list func-name)) " / ") pos) functions)))
      ;; Return functions, reversing the order
      (nreverse functions)))

  (defun amb/get-nesting-context (node)
    "Get the nesting context (class or function names) for a function."
    (let (context)
      (while (setq node (tsc-get-parent node))
        (cond
         ;; Capture class names for Python-like languages
         ((string= (tsc-node-type node) "class_definition")
          (push (tsc-node-text (tsc-get-child-by-field node :name)) context))
         ;; Capture outer function names
         ((string= (tsc-node-type node) "function_definition")
          (push (tsc-node-text (tsc-get-child-by-field node :name)) context))))
      context))

  (defun amb/setup-tree-sitter-imenu ()
    "Set up Imenu using Tree-sitter to extract function and method definitions."
    (if tree-sitter-mode
        (setq-local lsp-enable-imenu nil
                    lsp-ui-enable-imenu nil
                    imenu-create-index-function #'amb/tree-sitter-list-functions)
      (kill-local-variable 'imenu-create-index-function)))

  ;; Automatically enable for all tree-sitter-enabled modes
  (add-hook 'tree-sitter-mode-hook #'amb/setup-tree-sitter-imenu))

  (defun amb/show-nesting-context-at-point ()
    "Show the nesting context for the Tree-sitter node at point."
    (interactive)
    (let* ((node-at-point (tsc-get-descendant-for-position-range
                           (tsc-root-node tree-sitter-tree)
                           (point) (point)))
           (nesting-context (amb/get-nesting-context node-at-point)))
      (message "Nesting context: %s" (string-join nesting-context " / "))))

(map! :after python
      :map python-mode-map
      "C-c C-u" #'python-nav-backward-up-list)

(use-package! fennel-mode
  :config (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

(use-package! graphql-mode)

(after! alchemist-mode
  (map! (:when (modulep! :lang elixir)    ; local conditional
          (:map alchemist-mode-map
           :localleader
           "tt" #'exunit-toggle-file-and-test
           "tT" #'exunit-toggle-file-and-test-other-window))))

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
  :hook (yaml-mode . yaml-pro-mode)
  :hook (yaml-mode . yaml-pro-ts-mode)
  )

;; Set up typst-mode to associate with .typ files
(use-package! typst-mode
  :mode ("\\.typ\\'" . typst-mode)
  :init
  ;; Optional: set your typst formatting command if you want to use it
  (setq typst-format-command "typst fmt"))

;; Load ox-typst for Org-mode export to Typst documents
(use-package! ox-typst
  :after org
  :config
  ;; Optional: any custom configuration for ox-typst here
  )

;; TODO add ox-typst to list of Org export backends

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(use-package! janet-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-ts-mode)))

(use-package! flycheck-janet)

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

  (setq! magit-section-initial-visibility-alist '((stashes . show) (commits . show)))

  (defun amb/magit-checkout-default-branch ()
    "Check out the default branch of the current repository."
    (interactive)
    (let ((default-branch (magit-git-string "rev-parse" "--abbrev-ref" "origin/HEAD")))
      (when default-branch
        ;; Strip the 'origin/' part from the branch name
        (let ((branch (replace-regexp-in-string "^origin/" "" default-branch)))
          ;; Checkout the branch using Magit
          (magit--checkout branch)
          (magit-refresh)))))
  
  (transient-append-suffix 'magit-branch "b"
    '("M" "default branch" amb/magit-checkout-default-branch))

  (defun amb/copy-github-permalink ()
    "Generate a GitHub permalink for the current file at the current revision (full SHA).
  If a region is active, link to the highlighted line(s)."
    (interactive)
    (let* ((remote-url (magit-get "remote" (magit-get-remote) "url"))
           (repo-url (replace-regexp-in-string
                      (rx string-start
                          "git@"
                          (group (+ (not (any ":")))) ; match domain
                          ":"
                          (group (+ (not (any "."))))
                          (optional ".git")
                          string-end)
                      "https://\\1/\\2" remote-url))
           (full-sha (magit-rev-parse "HEAD"))
           (file-path (magit-file-relative-name buffer-file-name))
           (start-line (line-number-at-pos (region-beginning)))
           (end-line (line-number-at-pos (region-end)))
           (lines (if (use-region-p)
                      (if (= start-line end-line)
                          (format "#L%d" start-line)
                        (format "#L%d-L%d" start-line end-line))
                    ""))
           (permalink (format "%s/blob/%s/%s%s" repo-url full-sha file-path lines)))
      (copy-to-clipboard permalink)
      (message "GitHub permalink: %s" permalink))))

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

(defun amb/magit-stage-file ()
  (interactive)
  (if (magit-gitdir)
      (call-interactively #'magit-stage-file)
      (shell-command (concat
                      "git --git-dir=$HOME/.dots/ --work-tree=$HOME add "
                      (buffer-file-name))
                     t)))

(map! :after magit :leader "g S" #'amb/magit-stage-file)

;; all thanks and apologies to https://github.com/alphapapa/unpackaged.el
(use-package! smerge-mode
  :after (hydra magit)
  :config
  (defhydra amb/smerge-hydra
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
  (map! :leader :desc "resolve git conflicts" "gm" #'amb/smerge-hydra/body)
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (amb/smerge-hydra/body)))))

(after! (evil evil-collection)
  (add-to-list '+evil-collection-disabled-list 'info)
  (set-evil-initial-state! 'info-mode 'emacs))

(map! :map 'info-mode-map
      "j" #'next-line
      "k" #'previous-line)

(let ((dir "~/Dropbox/org/"))
  (and (file-exists-p dir)
       (setq org-directory dir)))

(setq! org-log-into-drawer t
       org-hierarchical-todo-statistics nil
       org-refile-use-outline-path 'full-file-path
       org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "BLOCKED(b)" "SOMEDAY(s)" "PROJ(p)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c)")))

(use-package! graphviz-dot-mode
  :after org)

(after! org
  (map! :after org
        :map 'org-mode-map
        "<tab>" #'org-cycle
        :nvie "C-M-S-RET" #'org-insert-todo-subheading
        :nvie "C-M-S-<return>" #'org-insert-todo-subheading
        :nvie "M-<return>" #'org-insert-heading)

  (defun my-org-mode-backtick-replacement ()
    "Replace a single backtick with = and triple backticks with a code block template."
    (interactive)
    (let ((context (buffer-substring-no-properties (max (point-min) (- (point) 2)) (point))))
      (if (string= context "==")
          (progn
            (delete-char -2)
            (insert "#+begin_src \n#+end_src\n")
            (forward-line -1)
            (move-beginning-of-line nil)
            (backward-char))
        (insert "="))))

  (defun my-org-mode-key-remap ()
    "Remap ` to custom function in org-mode."
    (local-set-key (kbd "`") 'my-org-mode-backtick-replacement))

  (add-hook 'org-mode-hook 'my-org-mode-key-remap))

(use-package! ox-gfm
  :after org)

(after! org
  (setq! org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args))))

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

(use-package! outshine
  :after org
  :config
  (add-hook 'prog-mode-hook 'outshine-mode))

;; should either of the following fns be wrapped in an `(after! org ...)` form?

(defun amb/org-todo-stats ()
  "Return statistics of TODO keywords in current buffer."
  (let ((todo-count 0)
        (done-count 0))
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when state
           (if (member state org-done-keywords)
               (cl-incf done-count)
             (cl-incf todo-count)))))
     t 'file)
    (format "[%d/%d]" done-count (+ todo-count done-count))))

(defun org-dblock-write:all-todo-stats (params)
  (insert (amb/org-todo-stats)))

(defun amb/window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `amb-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'amb-window-popup-frame)
    (delete-frame)))

(defmacro amb/define-window-popup-command (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `amb-window-popup-frame' parameter."
  `(defun ,(intern (format "amb/window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `amb-window-popup-frame' parameter.
Also see `amb/window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((amb-window-popup-frame . t)))))
       (select-frame frame)
       ;; buffers whose names start with a space char are considered hidden
       (switch-to-buffer " amb/window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(after! org
  (amb/define-window-popup-command org-capture)
  (add-hook 'org-capture-after-finalize-hook #'amb/window-delete-popup-frame))

(advice-add 'copy-unicode-char-to-clipboard :after 'amb/window-delete-popup-frame)
(amb/define-window-popup-command copy-unicode-char-to-clipboard)

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
 :desc "Spell checker" "tS" #'spell-fu-mode
 :desc "Subword mode" "ts" #'subword-mode)

(map!
 "C-;" #'evil-avy-goto-char-timer
 :ni "C-)" #'sp-forward-slurp-sexp
 :ni "C-(" #'sp-backward-slurp-sexp
 (:when (not (display-graphic-p)) :map (evil-insert-state-map evil-motion-state-map) "C-z" #'suspend-frame))

(setq user-full-name "Alex Birdsall"
      user-mail-address "ambirdsall@gmail.com")

(map! :leader
      :desc "open doom config" "F" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "open doom config" "fP" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "open computer-specific doom config" "fL" (cmd! (find-file amb/computer-specific-config)))

(defvar amb/computer-specific-config (expand-file-name "local-config.el" doom-private-dir)
  "A file for computer-specific config and overrides, hidden from git; for example,
configuration for a work computer and its (possibly private) product projects.")

(defvar amb/computer-specific-toggles (expand-file-name "local-toggles.el" doom-private-dir)
  "A file for {en,dis}abling configuration and features on a computer-specific basis,
hidden from git; for example, configuration for proprietary
features enabled for a work computer by a company account.")

(if (file-exists-p amb/computer-specific-toggles)
      (load amb/computer-specific-toggles))

(if (file-exists-p amb/computer-specific-config)
      (load amb/computer-specific-config))

(if (file-exists-p amb/computer-specific-config)
      (load amb/computer-specific-config))
