# Welcome! On a new computer, start by running `make install`.
# Once that is done, you can follow with any relevant system-dependent setup tasks
# (like `make x11` or `make macos`) as needed.

# Because `make install` sets up doom emacs, any downstream tasks can use
# ~/.emacs.d/bin/org-tangle to manage literate configurations.

.POSIX:
.SUFFIXES:
.PHONY: brew-deps defaults-write

install: .emacs.d .tmux/plugins/tpm c c/janet

.emacs.d:
	git clone --depth 1 git@github.com:doomemacs/doomemacs.git ~/.emacs.d
	echo "(doom! :config literate)" > ~/.config/doom/init.el
	~/.emacs.d/bin/doom install

.tmux/plugins/tpm:
	git clone git@github.com:tmux-plugins/tpm ~/.tmux/plugins/tpm
	~/.tmux/plugins/tpm/bin/install_plugins

linux: c/keyd

c:
	mkdir ~/c

c/keyd:
	~/.config/keyd/init.sh

c/janet:
	~/.config/janet/init.sh

macos: brew-deps defaults-write .hammerspoon

brew-deps:
	~/sbin/macos-install

defaults-write:
	~/sbin/macos-write-defaults

.hammerspoon:
	~/.config/hammerspoon/init_spoons.sh
