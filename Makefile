.POSIX:
.SUFFIXES:
install: .emacs.d
.emacs.d:
	git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d
