# mac uses BSD which, which acts nicely enough on its own and uses different
# options than the GNU coreutils one
if test -z "$IS_MAC"; then
    # For some reason this doesn't always override fedora's system alias—even though
    # manually running the underlying `unalias which` _does_ work. idk.
    #
    # Anyway, if fedora is yelling about a parse error downstream of a naming collision
    # here, try editing (using `sudo`) the file /etc/profile.d/which2.sh
    # emacs shortcut: /sudo:root@localhost:/etc/profile.d/which2.sh
    dots/clear_name which
    which() {
        (
            alias
            declare -f
        ) | command which --tty-only --read-alias --read-functions --show-tilde --show-dot $@
    }
fi
