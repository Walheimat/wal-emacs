#!/bin/bash

DIR="${0%/*}"

function no_init_file_exists() {
    local userdir=$1
    local method=$2

    if [[ -e "$userdir/init.el" || -e "$HOME/.emacs" ]]; then
        echo -e "\e[1;31m}< ,.__)\e[0m found existing init file, can't $method"
        return 1;
    fi

    return 0
}

function on_complete() {
    echo -e "\n\e[1;32m}< ,.__)\e[0m init file setup complete, you can restart Emacs"
}

function link_init_file() {
    local userdir="${1:-$HOME}"

    if no_init_file_exists "$userdir" "link"; then
        echo -e "\n(soft-) linking init file to ${userdir@Q}"
        ln -s "$(cd ..; pwd)/templates/init.el" "$userdir/init.el"
        on_complete
    fi
}

function copy_init_file() {
    local userdir="${1:-$HOME}"

    if no_init_file_exists "$userdir" "copy"; then
        echo -e "\ncopying init file to ${userdir@Q}"
        cp "$DIR/../templates/init.el" "$userdir/init.el"
        on_complete
    fi
}

echo -e "\e[1;34m}< ,.__)\e[0m \e[1m[init file setup]\e[0m"

func=$1

case $func in
    link)
        link_init_file "${@:2}"
        ;;
    copy)
        copy_init_file "${@:2}"
        ;;
    *)
        echo -e "\e[1;31m}< ,.__)\e[0m call with 'link' or 'copy'"
        ;;
esac
