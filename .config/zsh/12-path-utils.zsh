# mac uses BSD which, which acts nicely enough on its own and uses different
# options than the GNU coreutils one
if ! [ $IS_MAC ]; then
	dots/clear_name which
	which () {
	  (alias; declare -f) | command which --tty-only --read-alias --read-functions --show-tilde --show-dot $@
	}
fi
