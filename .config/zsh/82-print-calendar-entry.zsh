#!/usr/bin/env zsh

if [[ -f /usr/share/calendar/calendar.history ]]; then
    echo "Today in history:"
    grep $(date +'%m/%d') < /usr/share/calendar/calendar.history | shuf | head -n 1
    printf "\n"
fi
