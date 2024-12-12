#!/usr/bin/env bash

named_tab() {
    local name=$1
    local wd=$2
    local cmd=$3

    # ensure $name and $wd exist; $cmd is optional
    if [ -z "$name" ] || [ -z "$wd" ]; then
        echo "Usage: named_tab <name> <wd> [cmd]"
        return 1
    fi

    local pane_id=$(wezterm cli spawn --cwd $wd)
    wezterm cli set-tab-title --pane-id $pane_id "$name"

    if [ -n "$cmd" ]; then
        wezterm cli send-text --pane-id $pane_id "$cmd"
    fi

    # output the pane id for future reference
    echo $pane_id
}
