#!/bin/bash

MYDOTFILES=~/temp/my-dot-files
declare -a FILES_TO_BACKUP=(
    "~/.config/nvim/mysnippets"
    "~/.config/nvim/init.vim"
    "~/.bin/cpdotfiles.sh"
    "~/.bin/startup.sh"
    "~/.tmuxinator"
    "~/.tmux.conf"
    "~/.npmrc"
    "~/.zshrc"
)

function echom(){
    echo "$1 was copied!"
}

function expand_tilde(){
    echo "$HOME$(sed "s/^~\(.*\)/\1/" <<< $1)"
}

function backup(){
    if [[ -d "$MYDOTFILES" ]]; then
        for i in "${FILES_TO_BACKUP[@]}"
        do
            dest="$(sed 's/^~\/\(.*\)/\1/' <<< "$i")"
            dest="$MYDOTFILES/$dest"
            expanded_filename=$(expand_tilde $i)
            if [[ -f "$expanded_filename" ]]; then
                echo "filename: $expanded_filename"
                echo "dest: $dest"
                cp "$expanded_filename" "$dest"
            fi
            if [[ -d "$expanded_filename" ]]; then
                echo "dirname: $expanded_filename"
                echo "dest: $dest"
                cp -r "$expanded_filename"/* "$dest"
            fi
            echom "$i"
            echo
        done
    fi
}

backup
