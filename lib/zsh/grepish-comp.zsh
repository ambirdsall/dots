_grepish() {
    if ((CURRENT == 2)); then
        compadd $(cut -f 1 .git/tags tmp/tags 2>/dev/null | grep -v '!_TAG')
    fi
}
compdef _grepish ag rg ack grep
