#!/bin/fish

set fish_greeting

set -x PATH \
    ~/.npm-global/bin \
    ~/.nodebrew/current/bin \
    ~/.yarn-global/bin \
    ~/.ghcup/bin \
    $PATH

# Set alias
alias nvimtutor="nvim -c Tutor"
alias runuseful="docker run --rm -it -e TZ=/Asia/Tokyo -v $HOME:$HOME useful"

function dls
    set -l green (tput setaf 2)
    set -l default (tput sgr0)
    echo $green"[Images]"$default
    docker image ls
    echo
    echo $green"[Running containers]"$default
    docker container ls
end

function dla
    set -l green (tput setaf 2)
    set -l default (tput sgr0)
    echo $green"[All images (including intermediates)]"$default
    docker image ls --all
    echo
    echo $green"[All containers]"$default
    docker container ls --all
end

# Set tab step
tabs 3
