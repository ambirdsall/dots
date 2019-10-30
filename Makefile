.POSIX:
.SUFFIXES:
install: emacs
emacs: .emacs.d
	git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d
