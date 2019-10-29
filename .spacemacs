;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; * layers
(defun dotspacemacs/layers ()
;; ** framework configuration
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
;; ** layers
   dotspacemacs-configuration-layers '(csv
                                       ruby
                                       python
                                       sql
                                       yaml
                                       auto-completion
                                       better-defaults
                                       colors
                                       dash
                                       docker
                                       elixir
                                       elm
                                       emacs-lisp
                                       git
                                       github
                                       graphviz
                                       helm
                                       html
                                       javascript
                                       lsp
                                       markdown
                                       nginx
                                       (org :variables
                                            org-enable-bootstrap-support t
                                            org-enable-github-support t
                                            org-enable-reveal-js-support t
                                            org-hide-emphasis-markers t)
                                       osx
                                       pdf
                                       react
                                       (restclient :variables
                                                   restclient-use-org t)
                                       scala
                                       (shell :variables
                                              shell-default-height 30
                                              shell-default-position 'fullscreen
                                              shell-default-shell 'eshell)
                                       shell-scripts
                                       (spell-checking :variables
                                                       spell-checking-enable-by-default nil)
                                       spotify
                                       sql
                                       syntax-checking
                                       ;; themes-megapack
                                       theming
                                       tmux
                                       (typescript :variables
                                                   typescript-backend 'tide)
                                       version-control
                                       vimscript)
;; ** additional packages
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(ac-html-angular
                                      add-node-modules-path
                                      angular-mode
                                      angular-snippets
                                      bart-mode
                                      centaur-tabs
                                      dash-at-point
                                      doom-modeline
                                      doom-themes
                                      editorconfig
                                      evil-textobj-anyblock
                                      evil-textobj-line
                                      evil-replace-with-register
                                      evil-vimish-fold
                                      exec-path-from-shell
                                      exunit
                                      fireplace
                                      general
                                      jasminejs-mode
                                      lsp-scala
                                      nvm
                                      outshine
                                      ob-restclient
                                      ob-typescript
                                      origami
                                      ox-hugo
                                      ox-tufte
                                      sicp
                                      vimish-fold)
;; ** frozen packages
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
;; ** excluded packages
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(alchemist
                                    evil-search-highlight-persist
                                    spaceline)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

;; * dotspacemacs-variables
(defun dotspacemacs/init ()
;; ** preamble
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
;; ** le grand setq-default
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((bookmarks . 5)
                                (projects . 15)
                                (recents . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one
                         doom-nord-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "ᕙ(⇀‸↼‶)ᕗ"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 2
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; dotspacemacs-mode-line-theme (if (display-graphic-p) 'all-the-icons 'spacemacs)
   dotspacemacs-mode-line-theme 'vanilla
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

;; * $ENVIRONMENT_VARIABLES
(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (setenv "PKG_CONFIG_PATH" (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))
  (spacemacs/load-spacemacs-env))

;; * user-init
(defun dotspacemacs/user-init ()
;; ** preamble
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
;; ** language environments
;; *** typescript
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  ;; (setq hybrid-mode-enable-hjkl-bindings nil)

;; ** default frame size
  (when (display-graphic-p)
    (add-to-list 'default-frame-alist '(height . 72))
    (add-to-list 'default-frame-alist '(width . 150)))

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '("ensime" . "melpa-stable") package-pinned-packages)

;; ** clipboard integration (or insulation)
  ;; initially, and in general, the system clipboard should not get the kill-ring's dirty laundary
  (setq x-select-enable-clipboard nil)
  (setq select-enable-clipboard nil)
;; ** backup files
  (setq backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups")))))

;; ** package-specific vars
;; *** bart
  (setq bart-manage-window t)
  (setq bart-station '24th)
;; *** doc-view
  (setq doc-view-resolution 300)
  ) ;; end of defun
;; * dumping
(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

;; * user-config
(defun dotspacemacs/user-config ()
;; ** return of the macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (setq ns-function-modifier 'hyper)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;; ** better defaults
;; *** global defaults
  (setq-default major-mode 'org-mode)
  (setq auto-save-default nil)
  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
  (setq fill-column 100)
  (setq-default truncate-lines t)
;; *** dired
  (setq insert-directory-program (executable-find "gls"))
;; *** helm
  (setq helm-info-default-sources '(helm-source-info-emacs helm-source-info-elisp helm-source-info-org helm-source-info-magit))
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s --ignore-file '*/dist-*' ")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
;; *** company
  (setq company-tooltip-align-annotations t)
;; *** indentation
  (setq-default indent-tabs-mode nil)
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default standard-indent 2)
  (setq-default typescript-indent-level 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
;; ** require some generally-useful libraries just in case they're not around yet
  (require 'cl)
  (require 'dash)
  (require 's)
  (require 'f)
;; ** terminal
  ;; ITERM2 MOUSE SUPPORT
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)
    (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
    (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1))))

;; ** defuns
  (defun source-dotspacemacs-user-config ()
    (interactive)
    (dotspacemacs/user-config))

  (defun evil-open-above-without-leaving-normal-state (count)
    "Insert a new line above the current line and move the cursor to the new line without changing editing state."
    (interactive "p")
    (evil-open-above count)
    (normal-mode))

  (defun evil-open-below-without-leaving-normal-state (count)
    "Insert a new line below the current line and move the cursor to the new line without changing editing state."
    (interactive "p")
    (evil-open-below count)
    (normal-mode))

  (defun open-line-below ()
    (interactive)
    (end-of-line)
    (newline)
    (indent-for-tab-command))

  (defun open-line-above ()
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (indent-for-tab-command))

  (defun move-current-line (n)
    "Move the current line up or down by N lines."
    (interactive "p")
    (let ((col (current-column))
          start
          end)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (forward-char)
      (setq end (point))
      (let ((line-text (delete-and-extract-region start end)))
        (forward-line n)
        (insert line-text)
        ;; restore point to original column in moved line
        (forward-line -1)
        (forward-char col))))

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  (defun amb/fix-inline-images ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))

  (defun copy-filepath (relative-to-repo)
    "Copy the filename of the current buffer to the system clipboard, even if it's disabled."
    (interactive "P")
    (let ((select-enable-clipboard t)
          (filepath (when (buffer-file-name)
                      (if relative-to-repo
                          (f-relative (buffer-file-name) (projectile-project-root))
                        (buffer-file-name)))))
      (if (display-graphic-p)
          (when filepath (gui-select-text filepath))
        (when filepath (kill-new filepath)))))

  (defun copy-filename ()
    "Copy the filename of the current buffer to the system clipboard, even if it's disabled."
    (interactive)
    (let ((select-enable-clipboard t)
          (filename (f-filename (buffer-file-name))))
      (if (display-graphic-p)
          (gui-select-text filename)
        (when filename (kill-new filename)))))

  (defun amb/type-check-current-buffer-file ()
    "Pass the current buffer file to `tsc` for a type-checking pass.
Does not emit any compiled js."
    (interactive)
    (shell-command (s-concat "tsc --noEmit " (buffer-file-name))))

  (defun amb/org-insert-subheading-respect-content ()
    "Opens a new subheading without affecting the current line"
    (interactive)
    (call-interactively 'spacemacs/evil-insert-line-below)
    (call-interactively 'next-line)
    (call-interactively 'org-insert-subheading))

  (defun amb/keymap-symbol (keymap)
    "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
    (catch 'gotit
      (mapatoms (lambda (sym)
                  (and (boundp sym)
                       (eq (symbol-value sym) keymap)
                       (not (eq sym 'keymap))
                       (throw 'gotit sym))))))

  (defun amb/current-keymap ()
    "Print the name of the symbol to which the current keymap is bound, if any such symbol exists"
    (interactive)
    (if-let ((current-keymap (amb/keymap-symbol (current-local-map))))
        (message (symbol-name current-keymap))))

  (defun amb/what-the-face ()
    "Print the name of the face at point."
    (interactive)
    (message "The face at point is: %s" (or (face-at-point t) 'default)))

  ;; Avoid polluting the system clipboard
  (defun amb/toggle-clipboard ()
    "Toggles whether the system clipboard is accessable to emacs.

If it's connected, you can paste from the system clipboard, but all deleted or killed text will end
up polluting the system clipboard, which can get annoying fast.

If not, the system clipboard doesn't get polluted, but there's no great way to quickly grab text
from outside applications."
    (interactive)
    (if select-enable-clipboard
        (progn
          (setq select-enable-clipboard nil)
          (message "The system clipboard is safe and sound again!"))
      (progn
        (setq select-enable-clipboard t)
        (message "Copy and paste away, slugger!"))))

  (defun amb/paste-from-clipboard ()
    "Inserts the contents of the system clipboard at point."
    (interactive)
    (let ((select-enable-clipboard t))
      (call-interactively 'evil-paste-after)))

  (defun amb/evil-yank-to-clipboard ()
    "Ensures the system clipboard is enabled and then calls evil-yank.

Works with vim-style motions."
    (interactive)
    (let ((select-enable-clipboard t))
      (call-interactively 'evil-yank)))

  (defun amb/dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))

  (defun amb/prettify-region ()
    "Invoke the shell command prettier on region, replacing
contents with reformatted version.

This only works on self-contained semantic units, unfortunately: that is, you
can reformat a single class from a file, but not a single private method from a
class. \"Not working\" here means \"is replaced with an error message, not
a reformatted version of itself\"."
    (interactive)
    (let ((range-start (int-to-string (min (point) (mark))))
          (range-end (int-to-string (max (point) (mark)))))
      (shell-command
       (s-concat "prettier --range-start=" range-start " --range-end=" range-end " --parser typescript --use-tabs --trailing-comma 'all' " (buffer-file-name))
       (current-buffer))))

  (defun amb/copy-file-path-relative-to-project-root ()
    "Put the current buffer's filepath relative to the project root in the kill ring. Good for imports"
    (interactive)
    ;; TODO if text is selected, add it into the saved string as the imported object
    ;; TODO select buffer with helm and insert in current buffer, respecting aliases and resolving
    ;; relative to the current buffer if they're in the same subproject
    (let* ((relative-path (f-relative (buffer-file-name) (projectile-project-root)))
           (trimmed-path (replace-regexp-in-string "\.ts$" "" relative-path))
           )
      (kill-new trimmed-path)))

  (defun comment-auto-fill ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))

  (defun amb/fill-or-unfill ()
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'endless/fill-or-unfill)
               (progn (setq this-command nil)
                      (point-max))
             fill-column)))
      (call-interactively #'fill-paragraph)))

  (defun amb/jump-around (restrict-to-open-buffers)
    "Grab the helm and go to a project file quickly.

If called with a prefix arg, restricts to open buffers; by default, any file."
    (interactive "P")
    (if restrict-to-open-buffers
        (call-interactively #'helm-buffers-list)
      (if (projectile-project-p)
          (call-interactively #'helm-projectile-find-file)
        (call-interactively #'helm-find-files))))

  (defun amb/html2org-clipboard ()
    "Convert clipboard contents from HTML to Org and then paste (yank)."
    (interactive)
    (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org --wrap=none"))
    (yank))

  (defun dired-projectile-project-root ()
    "If in a projectile project, open a dired buffer in the project root directory."
    (interactive)
    (and (projectile-project-p) (dired (projectile-project-root))))
;; *** predicate?
  ;; I just really vastly prefer "use question mark as suffix" as an idiom for
  ;; type predicates to "use 'p' as a suffix except when it looks really weird or
  ;; would be misleading because there exists some other word"
  (defalias 'atom? 'atom)
  (defalias 'buffer? 'bufferp)
  (defalias 'cons? 'consp)
  (defalias 'display-graphic? 'display-graphic-p)
  (defalias 'frame? 'framep)
  (defalias 'list? 'listp)
  (defalias 'null? 'null)
  (defalias 'number? 'numberp)
  (defalias 'process? 'processp)
  (defalias 'string? 'stringp)
  (defalias 'subr? 'subrp)
  (defalias 'symbol? 'symbolp)
  (defalias 'vector? 'vectorp)
  (defalias 'window? 'windowp)

  ;; Hell, it's just better for predicates generally
  (defalias 'bound? 'boundp)
  (defalias 'fbound? 'fboundp)
  (defalias 'use-region? 'use-region-p)

;; *** tab wrangling
  (defun amb/tabify-buffer ()
    "tabify current buffer, the whole current buffer, and nothing but the current buffer."
    (interactive)
    (save-excursion (tabify (point-min) (point-max))))

  (defun amb/untabify-buffer ()
    "untabify current buffer, the whole current buffer, and nothing but the current buffer."
    (interactive)
    (save-excursion (untabify (point-min) (point-max))))
;; *** file wrangling
  (defun amb/find-alternate-file ()
    "Tries to find a conventionally-located test file based on the current file's filename and location."
    (interactive)
    (cond
     ((s-matches? ".scala$" (buffer-file-name))
      (if (s-matches? "Spec.scala" (buffer-file-name))
          (let ((implementation-file-path (s-replace-all '(("Spec.scala" . ".scala") ("test" . "main")) (buffer-file-name))))
            (if (f-exists? implementation-file-path)
                (find-file implementation-file-path)
              (let ((backup-path-for-copilot-backend-smh (s-replace "server/api/" "server/api/route/" implementation-file-path)))
                (if (f-exists? backup-path-for-copilot-backend-smh)
                    (find-file backup-path-for-copilot-backend-smh)
                  (message (s-concat "could not find that fucker at " implementation-file-path " or " backup-path-for-copilot-backend-smh))))))
        (let ((spec-file-path (s-replace-all '((".scala" . "Spec.scala") ("/main/" . "/test/")) (buffer-file-name))))
          (if (f-exists? spec-file-path)
              (find-file spec-file-path)
            (let ((backup-path-for-copilot-backend-smh (s-replace "route/" "" spec-file-path)))
              (if (f-exists? backup-path-for-copilot-backend-smh)
                  (find-file backup-path-for-copilot-backend-smh)
                (if (y-or-n-p (s-concat "could not find that fucker " spec-file-path ". Create it?"))
                    (find-file spec-file-path))))))))
     ((t (message "don't know how to find the alternate file for this file type")))))

  (defun amb/touch-current-file ()
    "does what it says on the tin, where `touch' refers to the shell command."
    (interactive)
    (shell-command (s-concat "touch " (buffer-file-name))))
;; *** find-file-as-command commands
  (defmacro find-file-as-command (filename)
    `(lambda (P-is-for-prefix-arg)
       (interactive "P")
       (if current-prefix-arg (split-window-right-and-focus))
       (find-file ,filename)))

  (cl-flet ((amb/ (lambda (filename) (s-concat user-emacs-directory "amb/" filename))))
    (fset 'amb/edit-elisp-notes (find-file-as-command (amb/ "elisp.org"))))
  (fset 'amb/edit-cli-primer (find-file-as-command "~/notes/cli-primer.org"))
  (fset 'amb/open-agenda-file (find-file-as-command "~/notes/agenda.org"))
  (fset 'amb/edit-sigfig-notes (find-file-as-command "~/notes/sigfig.org"))
  (fset 'amb/edit-org-mode-glossary-notes (find-file-as-command "~/notes/org-mode-glossary.org"))

  (defmacro helm-edit-file-from-directory (helm-title dir)
    `(lambda (_prefix)
       (interactive "P")
       (helm :sources `((name . ,,helm-title)
                        (candidates . ,(-map (lambda (f)
                                               `(,(f-relative f ,dir) . ,f))
                                             (f-files ,dir
                                                      (lambda (g) (not (s-matches? "\.DS_Store" g))))))
                        (action . (lambda (c)
                                    (if current-prefix-arg (split-window-right-and-focus))
                                    (find-file c)))))))

  (fset 'amb/pick-a-note-why-dont-ya (helm-edit-file-from-directory "NOTES" "/Users/abirdsall/notes"))

  (defmacro on-string-or-region (fn)
    "Given a string-manipulation function, defines an interactive command which will apply that
function to either a string argument or to selected text, depending on context."
    `(lambda (string &optional from to)
       (interactive
        (if (use-region?)
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

;; *** dead sea scrolling
  (defun amb/scroll-down ()
    "scroll down a line, for the definition of \"down\" I find intuitive."
    (interactive)
    (scroll-up 1))

  (defun amb/scroll-up ()
    "scroll up a line, for the definition of \"up\" I find intuitive."
    (interactive)
    (scroll-down 1))
;; ** git
;; *** general magit UI
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; *** github enterprise tho
  ;; Make magit-gh-pulls look at the correct URL for GHE
  ;; This is an incomplete solution; it appears the library hardcodes the github URL
  (setq gh-profile-alist '(("github"
                            :url "https://git.sigfig.com/api/v3"
                            :remote-regexp "^\\(?:git@git\\.sigfig\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?git\\.sigfig\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")))

  (defun amb/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://git.sigfig.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+git\\.sigfig\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch))))

  (with-eval-after-load 'forge
    (push '("git.sigfig.com" "git.sigfig.com/api"
                                      "git.sigfig.com" forge-github-repository) forge-alist))

  ;; use emacs for command line git stuff
  (global-git-commit-mode)

;; ** hooks
  (add-hook 'dired-mode-hook 'amb/dired-mode-setup)
  (add-hook 'org-babel-after-execute-hook 'amb/fix-inline-images)
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'comment-auto-fill)
  (add-hook 'js2-mode-hook (lambda () (jasminejs-mode)))
  (add-hook 'jasminejs-mode-hook (lambda () (jasminejs-add-snippets-to-yas-snippet-dirs)))
  (add-hook 'typescript-mode-hook 'setup-tide-mode)

  ;; paredit!
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; tagedit!
  (and (functionp 'tagedit-mode) (add-hook 'web-mode-hook #'tagedit-mode))


;; ** evil
;; *** evil-replace-with-register
  (require 'evil-replace-with-register)
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install)

;; ** company-mode
  (global-company-mode)
  (company-tng-configure-default)

;; ** .dir-locals.el behavior
  ;; hitting "y" once is plenty, thanks
  (advice-add 'risky-local-variable-p :override #'ignore)

;; ** eshell
;; *** defuns
  (defun root ()
    "Returns as a string the name of the root directory or the filesystem root, whichever comes first.

If you're anywhere but the top-level directory inside of a
projectile project, it returns the project root directory. If the
current working directory already is the project root or you're
outside of a projectile project, returns either the nearest
parent directory that's a projectile root directory or the
filesystem root, whichever comes first."
    (or (projectile-project-root) "/"))
;; ** org it up
;; *** defuns
  (defun amb/insert-jira-ticket-org-link (ticket-id)
    "inserts an org link to the given jira ticket at point, with `ticket-id' as the visible text.

`ticket-id' is a jira number like COP-123 or MRY-444 or whatever."
    (interactive (list (read-string "Jira ticket: ")))
    (insert (s-concat "[[//jira.sigfig.com/browse/" ticket-id "][" ticket-id "]]")))

  (defun amb/org-new-subheading (is-todo)
    (interactive "P")
    (if is-todo
        (progn
          (amb/org-insert-subheading-respect-content)
          (org-todo))
      (amb/org-insert-subheading-respect-content)))

  (defun amb/org-new-heading (is-todo)
    (interactive "P")
    (if is-todo
        (progn
          (org-insert-heading-after-current)
          (org-todo))
      (org-insert-heading-after-current)))

;; *** outshine
    (require 'outshine)

    (add-hook 'prog-mode-hook 'outshine-mode)

    ;; Narrowing now works within the headline rather than requiring to be on it
    (advice-add 'outshine-narrow-to-subtree :before
                (lambda (&rest args) (unless (outline-on-heading-p t)
                                       (outline-previous-visible-heading 1))))

;; *** agenda
    ;; Add projectile TODO.org files to agenda automatically
    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (append (org-projectile-todo-files) org-agenda-files)
      (push "~/notes/agenda.org" org-agenda-files)
      (push "~/notes/ical-entries.org" org-agenda-files))

;; *** language support and export formats
    (with-eval-after-load 'org
      (require 'ox-beamer)
      (require 'ox-confluence)
      (require 'ox-tufte)
      (require 'ob-dot)
      (require 'ob-js)
      (require 'ob-restclient)
      (require 'ob-ruby)
      (require 'ob-shell)
      (require 'ob-typescript)
      (setq org-export-babel-evaluate nil)
      ;; Set sensible mode for editing dot files
      (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
      ;; Update images from babel code blocks automatically
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
      (setq org-src-fontify-natively t)
      (setq org-src-tab-acts-natively t)
      (setq org-confirm-babel-evaluate nil)
      (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((dot . t)
         (js . t)
         (restclient . t)
         (ruby . t)
         (shell . t)
         (typescript . t))))

  (use-package ox-tufte
    :ensure t
    :after ox)

;; ** paint me like one of your french editors
;; *** Get rid of some dated-ass default UI
    (scroll-bar-mode -1)
    (tool-bar-mode   -1)
    (tooltip-mode    -1)
    (menu-bar-mode   -1)

;; *** frame appearance
    (when (eq system-type 'darwin)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; *** whitespace
    (require 'whitespace)
    (setq whitespace-display-mappings
          ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
          '(
            (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
            (newline-mark 10 [182 10]) ; 10 LINE FEED
            (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
            ))
    (setq whitespace-style '(face tabs trailing tab-mark))

    ;; set to dark by default, same as the actual themes
    (set-face-attribute 'whitespace-tab nil
                        :background "#272727"
                        :foreground "#383838"
                        :weight 'normal)
    (set-face-attribute 'whitespace-trailing nil
                        :background "#e4eeff"
                        :foreground "#183bc8"
                        :weight 'normal)
    (add-hook 'prog-mode-hook 'whitespace-mode)

    ;; and keep this ish in sync with the theme, at least for standard theme switching via 'spacemacs/cycle-spacemacs-theme
    (defun amb/set-tab-color ()
      (if (eq (frame-parameter nil 'background-mode) 'dark)
          (set-face-attribute 'whitespace-tab nil
                              :background "#272727"
                              :foreground "#383838"
                              :weight 'normal)
        (set-face-attribute 'whitespace-tab nil
                            :background "#DBDFE6" ; (dolist n '(224 228 235) (* 0.98 n))
                            :foreground "#EAEEF5"
                            :weight 'normal)))
    (advice-add 'spacemacs/theme-transient-state/spacemacs/cycle-spacemacs-theme :after #'amb/set-tab-color)
    (advice-add 'spacemacs/cycle-spacemacs-theme :after #'amb/set-tab-color)

;; *** modeline
    (require 'doom-modeline)
    (doom-modeline-init)
    (setq doom-modeline-height 23)

;; *** mode-line
  (spacemacs/toggle-mode-line-battery-on)

;; *** rainbow mode
  (with-eval-after-load 'rainbow-mode
    (setq old-rainbow-html-colors-major-mode-list rainbow-html-colors-major-mode-list)
    (setq rainbow-html-colors-major-mode-list (cons 'scss-mode rainbow-html-colors-major-mode-list)))

;; *** treemacs
  (with-eval-after-load 'treemacs
    (treemacs-resize-icons 13))
;; *** centaur tabs
  (centaur-tabs-mode t)
;; ** programming language environments
;; *** elisp
;; **** lord help me, I need a better tabs vs spaces system
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil)))
;; *** scala
  (setq ensime-startup-notification nil)

;; *** typescript
  ;; Fix errors resulting from tempfiles in the source directory
  (with-eval-after-load 'typescript-tslint
    (setcar (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command))
            'source-original))

  ;; ** tags and shit
  ;; I never want to open ETAGS in the editor; I want to think of it purely as a support buffer
(and (boundp 'spacemacs-useless-buffers-regexp) (push "ETAGS" spacemacs-useless-buffers-regexp))

  ;;;;;;;;;;;;;;;
  ;; PROJECTILE / TAGS
  ;; Copyright (c) 2016, Matthew Weigel, cf. https://github.com/bsdcat/effortless_git_tags
  (defun find-git-repo-tags-file ()
    "Find a TAGS file (as ETAGS) if the current buffer is in a git repository."
    (when
        (and (buffer-file-name) (vc-git-root (buffer-file-name))
             (file-readable-p (expand-file-name ".git/ETAGS" (vc-git-root (buffer-file-name)))))
      (expand-file-name ".git/ETAGS" (vc-git-root (buffer-file-name)))))

  (defvar default-tags-table-function 'find-git-repo-tags-file)

;; *** elixir
  (use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "/Users/abirdsall/code/elixir/elixir-ls/release"))

  (with-eval-after-load 'elixir-mode
    (spacemacs/declare-prefix-for-mode 'elixir-mode
      "mt" "tests" "testing related functionality")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "tb" 'exunit-verify-all
      "ta" 'exunit-verify
      "tk" 'exunit-rerun
      "tt" 'exunit-verify-single))
;; ** web-mode
  (setq web-mode-engines-alist
        '(("angular" . "\\.html")
          ("phoenix" . "\\.html.eex")))
;; ** editorconfig
  (use-package editorconfig
    :ensure t
    :defer t
    :config
    (editorconfig-mode 1))

;; ** load keybindings last, in hopes that they won't be overwritten
  (require 'general)
;; *** folding systems
;; **** org/outshine
(if (display-graphic-p) ;; org-mode heading keybindings
    (with-eval-after-load 'org
      (define-key org-mode-map (kbd "C-RET") #'amb/org-new-heading)
      (define-key org-mode-map (kbd "C-S-RET") #'amb/org-new-subheading)
      ;; (define-key evil-org-mode-map (kbd "<down>") #'next-line) ;; keymap overridden :(
      ;; (define-key evil-org-mode-map (kbd "<up>") #'previous-line) ;; keymap overridden :(
      (define-key org-mode-map [remap evil-org-org-insert-todo-heading-respect-content-below] 'amb/org-new-subheading)
      ;; (evil-define-key 'normal org-mode-map
      ;;   (kbd "C-RET") #'amb/org-new-heading
      ;;   (kbd "C-S-RET") #'amb/org-new-subheading)
      )
  (with-eval-after-load 'org
    (let ((map (if (boundp 'input-decode-map)
                   input-decode-map
                 function-key-map)))
      (define-key map "\e[1;P9" (kbd "C-RET"))
      (define-key org-mode-map (kbd "C-RET") #'amb/org-new-heading)
      (define-key map "\e[1;P10" (kbd "C-S-RET"))
      (define-key org-mode-map (kbd "C-S-RET") #'amb/org-new-subheading)
      (define-key org-mode-map [remap evil-org-org-insert-todo-heading-respect-content-below] 'amb/org-new-subheading)
      ; (evil-define-key 'normal org-mode-map $
      ;   (kbd "C-RET") #'amb/org-new-heading $
      ;   (kbd "C-S-RET") #'amb/org-new-subheading) $
      )))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "g" #'amb/edit-org-mode-glossary-notes)

(spacemacs/set-leader-keys
  ;; Narrowing
  "nn" 'outshine-narrow-to-subtree
  "nw" 'widen

  ;; Structural edits
  "nj" 'outline-move-subtree-down
  "nk" 'outline-move-subtree-up
  "nh" 'outline-promote
  "nl" 'outline-demote)

;; TODO: use outshine map instead
(let ((kmap outline-minor-mode-map))
  (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
  (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key '(normal visual motion) kmap
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level))
;; **** vimish-fold
(require 'vimish-fold)
(require 'evil-vimish-fold)
(evil-vimish-fold-mode 1)
;; *** undo in region in visual mode
(evil-global-set-key 'visual "u" 'undo-tree-undo)
;; *** C-return/C-S-return == vim-style o/O
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; *** arrow keys, amirite?
(evil-global-set-key 'normal (kbd "<down>") 'amb/scroll-down)
(evil-global-set-key 'visual (kbd "<down>") 'amb/scroll-down)
(evil-global-set-key 'motion (kbd "<down>") 'amb/scroll-down)
(evil-global-set-key 'normal (kbd "<up>") 'amb/scroll-up)
(evil-global-set-key 'visual (kbd "<up>") 'amb/scroll-up)
(evil-global-set-key 'motion (kbd "<up>") 'amb/scroll-up)

;; *** vim-style tab navigation
(evil-global-set-key 'normal (kbd "g t") 'centaur-tabs-forward)
(evil-global-set-key 'normal (kbd "g T") 'centaur-tabs-backward)

;; *** global application shortcuts to match OS keybindings
(global-set-key (kbd "M-s-SPC") (lambda () (interactive) (shell-command "open '/Applications/Google Chrome.app'")))
;; s-SPC not always getting recognized tho
(global-set-key (kbd "s-SPC") (lambda () (interactive) (shell-command "open '/Applications/iTerm.app")))

(global-set-key [remap fill-paragraph]
                #'amb/fill-or-unfill)

;; *** paredit
;; limited global paredit
(global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-(") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-}") 'paredit-backward-barf-sexp)
;; TODO: whyyyyyy is this not working tho
;; (define-key web-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
;; (define-key web-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)

;; *** doc-view mode
(with-eval-after-load 'doc-view
  ;; fix clobbered keybindings
  (define-key doc-view-mode-map (kbd "n") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "l") 'doc-view-next-page))

;; *** leader keybindings
;; **** 'd'
(spacemacs/declare-prefix "d" "dash")
;; **** 'o'
(spacemacs/declare-prefix "o" "ᕙ(⇀‸↼‶)ᕗ")
(spacemacs/declare-prefix "oe" "edit note files")
(spacemacs/set-leader-keys
  "oec" #'amb/edit-cli-primer
  "oee" #'amb/pick-a-note-why-dont-ya
  "oeE" #'amb/edit-elisp-notes
  "oes" #'amb/edit-sigfig-notes
  )

(spacemacs/declare-prefix "ot" "typescript")
(spacemacs/set-leader-keys
  "otc" #'tide-jsdoc-template
  "ott" #'amb/type-check-current-buffer-file
  "otp" #'amb/prettify-region
  "oty" #'amb/copy-file-path-relative-to-project-root)

(spacemacs/declare-prefix "oh" "help")
(spacemacs/set-leader-keys
  "ohk" #'which-key-show-major-mode
  "ohK" #'which-key-show-keymap)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxt" #'amb/tabify-buffer
  "oxT" #'amb/untabify-buffer)
;; **** 'z'
(spacemacs/declare-prefix "zo" "origami")
(spacemacs/set-leader-keys
  "zoc" #'origami-close-all-nodes
  "zoo" #'origami-open-all-nodes)
;; **** parens
(spacemacs/declare-prefix ")" "paredit ⇨")
(spacemacs/set-leader-keys
  ")s" #'paredit-forward-slurp-sexp
  ")b" #'paredit-forward-barf-sexp)

(spacemacs/declare-prefix "(" "paredit ⇦")
(spacemacs/set-leader-keys
  "(s" #'paredit-backward-slurp-sexp
  "(b" #'paredit-backward-barf-sexp)
;; **** 'H'
(spacemacs/declare-prefix "H" "helm")
(spacemacs/set-leader-keys
  "Hf" #'helm-find-files
  "Hp" #'helm-projectile-find-file
  "Hm" #'helm-multi-files
  "Hs" #'helm-swoop
  "HS" #'helm-multi-swoop-all
  "Hi" #'helm-semantic-or-imenu
  "Hg" #'helm-grep-do-git-grep)
;; **** the catchall spacemacs/set-leader-keys
(spacemacs/set-leader-keys
  ":"    #'helm-M-x
  "."    (lambda () (interactive) (dired "."))
  "/"    #'spacemacs/helm-project-do-rg
  "SPC"  #'amb/jump-around
  "aD"   #'dired-projectile-project-root
  "D"    #'docker
  "fa"   #'amb/find-alternate-file
  "ft"   #'amb/touch-current-file
  "fer"  #'source-dotspacemacs-user-config
  "gg"   #'magit-dispatch ;; adios, gist prefix :/
  "G"    #'magit-status
  "gP"   #'amb/visit-pull-request-url
  "L"    #'spacemacs/workspaces-transient-state/body
  "l"    #'spacemacs/layouts-transient-state/body
  "nk"   #'evil-numbers/inc-at-pt
  "nj"   #'evil-numbers/dec-at-pt
  "oa"   #'org-agenda
  "oA"   #'amb/open-agenda-file
  "oc"   #'amb/toggle-clipboard
  "od"   #'delete-trailing-whitespace
  "of"   #'evil-first-non-blank
  "oF"   #'font-lock-fontify-buffer
  "ol"   #'evil-buffer
  "oo"   #'evil-open-below-without-leaving-normal-state
  "oO"   #'evil-open-above-without-leaving-normal-state
  "op"   #'amb/paste-from-clipboard
  "oy"   #'amb/evil-yank-to-clipboard
  "oz"   #'evil-toggle-fold
  "po"   #'org-projectile/goto-todos
  "pO"   #'org-projectile-goto-location-for-project
  "ps"   #'amb/helm-ag-in-projectile-root
  "hf"   #'describe-function
  "hF"   #'amb/what-the-face
  "hh"   #'describe-key-briefly
  "hm"   #'woman
  "hk"   #'describe-key
  "hK"   #'which-key-show-top-level
  "hv"   #'describe-variable
  "J"    #'evil-avy-goto-char-timer
  "jj"   #'evil-avy-goto-char-2
  "jJ"   #'evil-avy-goto-char
  "si"   #'helm-imenu
  "V"    #'er/contract-region
  "ww"   #'ace-window
  "wW"   #'other-window
  "W"    #'subword-mode
  "xF"   #'unfill-paragraph
  "zO"   #'origami-mode
  "Z"    #'evil-toggle-fold)
;; *** TODO catchall section, refactor
;; **** arrow keys in isearch
(progn ;; jump between isearch results using arrow keys.
  ;; left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))

;; **** dired
(with-eval-after-load 'dired
  (evilified-state-evilify dired-mode dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

(progn ;; insert current filename into minibuffer (e.g. for shell command)
  (define-key minibuffer-local-map
    [f3] (lambda () (interactive)
           (insert (buffer-file-name (current-buffer-not-mini)))))

  (defun current-buffer-not-mini ()
    "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
    (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
          (window-buffer (previous-window)) (window-buffer (next-window))))))

;; **** search navigation
(progn
 (define-key evil-normal-state-map (kbd "gf") #'helm-find-files)
 (define-key evil-normal-state-map (kbd "/") #'helm-swoop)
 (define-key evil-visual-state-map (kbd "/") #'helm-swoop))

;; **** Info-mode scrolling
(progn
  (define-key Info-mode-map (kbd "<up>") #'evil-scroll-line-up)
  (define-key Info-mode-map (kbd "<down>") #'evil-scroll-line-down)
  (define-key Info-mode-map (kbd "<left>") #'Info-backward-node)
  (define-key Info-mode-map (kbd "<right>") #'Info-forward-node))

;; **** org
(with-eval-after-load 'org
 (define-key org-mode-map (kbd "<up>") #'previous-line)
 (define-key org-mode-map (kbd "<down>") #'next-line)
 ;; (define-key org-mode-map (kbd "<") #'evil-shift-left)
 ;; (define-key org-mode-map (kbd ">") #'evil-shift-right)
 (define-key org-mode-map (kbd "<C-S-return>") #'amb/org-insert-subheading-respect-content)
 (spacemacs/set-leader-keys-for-major-mode 'org-mode
   "hs" #'amb/org-insert-subheading-respect-content
   "hi" #'org-insert-heading-after-current
   "p" #'amb/html2org-clipboard))

;; TODO: if on a mac and GUI, bind `(kbd "s-_")` (i.e. alt-shift-dash, the standard OS-level em-dash binding) to self-insert em-dash
)
;; * automatically-inserted shit
dotspacemacs-configuration-layers
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(delete-by-moving-to-trash nil)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#eee8d5")
 '(fill-column 100)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-html-doctype "html5")
 '(package-selected-packages
   (quote
    (helm-navi ac-html-angular angular-mode outshine clj-refactor edn clojure-snippets paredit peg cider-eval-sexp-fu cider queue clojure-mode ob-typescript org-category-capture ox-tufte discover cargo toml-mode racer flycheck-rust seq rust-mode racket-mode faceup groovy-mode typit mmt sudoku pacmacs 2048-game yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic graphviz-dot-mode fireplace magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht ranger ob-sml sml-mode origami spotify helm-spotify multi powerline rake inflections pcre2el spinner osc log4e gntp skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors hydra parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flx iedit anzu goto-chg undo-tree highlight f diminish autothemer web-completion-data dash-functional tern bind-map bind-key packed elixir-mode pkg-info epl avy auto-complete popup geiser csv-mode typescript-mode sbt-mode scala-mode inf-ruby company smartparens evil flycheck helm helm-core markdown-mode alert org-plus-contrib magit magit-popup git-commit with-editor async yasnippet php-mode js2-mode dash s define-word zonokai-theme zenburn-theme zen-and-art-theme xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline spacegray-theme soothe-theme sonic-pi solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme purple-haze-theme pug-mode projectile-rails professional-theme popwin planet-theme phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy pastels-on-dark-theme paradox ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-tree-slide org-projectile org-present org-pomodoro org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-elixir noflet noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme info+ indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy flyspell-correct-helm flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-replace-with-register evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav editorconfig dumb-jump drupal-mode dracula-theme django-theme diff-hl darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cyberpunk-theme company-web company-tern company-statistics company-shell column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values
   (quote
    ((projectile-tags-file-name . "~/workspace/ngts/.git/ETAGS")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(delete-by-moving-to-trash nil)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#eee8d5")
 '(fill-column 100)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f"))))
 '(js-indent-level 2 t)
 '(magit-diff-use-overlays nil)
 '(max-specpdl-size 9000)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-html-doctype "html5")
 '(package-selected-packages
   (quote
    (seeing-is-believing ruby-refactor ruby-hash-syntax rubocopfmt helm-gtags ggtags enh-ruby-mode dap-mode bui tree-mode counsel-gtags counsel swiper ivy helm-navi ac-html-angular angular-mode outshine clj-refactor edn clojure-snippets paredit peg cider-eval-sexp-fu cider queue clojure-mode ob-typescript org-category-capture ox-tufte discover cargo toml-mode racer flycheck-rust seq rust-mode racket-mode faceup groovy-mode typit mmt sudoku pacmacs 2048-game yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic graphviz-dot-mode fireplace magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht ranger ob-sml sml-mode origami spotify helm-spotify multi powerline rake inflections pcre2el spinner osc log4e gntp skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors hydra parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flx iedit anzu goto-chg undo-tree highlight f diminish autothemer web-completion-data dash-functional tern bind-map bind-key packed elixir-mode pkg-info epl avy auto-complete popup geiser csv-mode typescript-mode sbt-mode scala-mode inf-ruby company smartparens evil flycheck helm helm-core markdown-mode alert org-plus-contrib magit magit-popup git-commit with-editor async yasnippet php-mode js2-mode dash s define-word zonokai-theme zenburn-theme zen-and-art-theme xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline spacegray-theme soothe-theme sonic-pi solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme purple-haze-theme pug-mode projectile-rails professional-theme popwin planet-theme phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy pastels-on-dark-theme paradox ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-tree-slide org-projectile org-present org-pomodoro org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-elixir noflet noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme info+ indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy flyspell-correct-helm flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-replace-with-register evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav editorconfig dumb-jump drupal-mode dracula-theme django-theme diff-hl darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cyberpunk-theme company-web company-tern company-statistics company-shell column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values
   (quote
    ((projectile-tags-file-name . "~/workspace/ngts/.git/ETAGS")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(typescript-indent-level 2 t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-implicit-highlight ((t (:underline nil)))))
)
