.POSIX:
.SUFFIXES:
install: .emacs.d tpm

.emacs.d:
	git clone -b develop --depth 1 https://github.com/hlissner/doom-emacs.git ~/.emacs.d
	~/.emacs.d/bin/doom install

tpm:
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
