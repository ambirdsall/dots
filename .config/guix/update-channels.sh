#!/bin/sh

guix pull --channels=$HOME/.config/guix/my-channels.scm
guix describe --format=channels > ~/.config/guix/channels.scm
