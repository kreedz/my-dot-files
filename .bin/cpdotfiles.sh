#!/bin/bash

function echom(){
    echo "$1 was copied!"
}

MYDOTFILES=~/temp/my-dot-files

cp ~/.config/nvim/init.vim $MYDOTFILES/.config/nvim
echom "init.vim"

cp ~/.zshrc $MYDOTFILES
echom ".zshrc"


cp ~/.bin/cpdotfiles.sh $MYDOTFILES/.bin/
echom "cpdotfiles.sh"
