if dots/at_hand rbenv; then
    [ $IS_MAC ] && export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
    eval "$(rbenv init -)"
fi

alias be="bundle exec"

bo () {
    emv $(bundle show "$1")
}
