if [ $(uname) == 'Darwin' ]; then
    h () {
        local cols sep
        cols=$(( COLUMNS / 3 ))
        sep='{::}'

        # TODO: find chrome history's location on linux
        cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

        sqlite3 -separator $sep /tmp/h \
                "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
            awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
            fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
    }
fi
