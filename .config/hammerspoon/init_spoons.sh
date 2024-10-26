#!/usr/bin/env bash

# Script to clone spoon repositories and define the necessary remotes for contributing
hammer="$HOME/.hammerspoon"

if [ ! -d "$hammer" ]; then
    ln -s "$HOME/.config/hammerspoon" "$hammer"
fi

mkdir -p $hammer/Spoons

git clone git@github.com:ambirdsall/PaperWM.spoon.git $hammer/Spoons/PaperWM.spoon
cd $hammer/Spoons/PaperWM.spoon
git remote add origin git@github.com:ambirdsall/PaperWM.spoon.git
git remote add origin git@github.com:ambirdsall/PaperWM.spoon.git
git remote add upstream https://github.com/mogenson/PaperWM.spoon
git remote add upstream https://github.com/mogenson/PaperWM.spoon

cd $hammer
git clone git@github.com:ambirdsall/spacehammer.git $hammer/Spoons/Spacehammer.spoon
cd $hammer/Spoons/Spacehammer.spoon
git remote add agzam git@github.com:agzam/spacehammer.git
git remote add agzam git@github.com:agzam/spacehammer.git
git remote add jaide https://github.com/jaidetree/spacehammer.git
git remote add jaide https://github.com/jaidetree/spacehammer.git
git remote add origin git@github.com:ambirdsall/spacehammer.git
git remote add origin git@github.com:ambirdsall/spacehammer.git

