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

if command -sq docker
  alias runu="docker run --rm -itd -v $HOME:$HOME useful"

  function econ
    set -l container_id $argv[1]
    if echo $container_id | grep -qE '^ *+$'
      echo Specify the container ID that you want to enter!
    else
      docker exec -it $container_id bash
    end
  end

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
end

# Set tab step
tabs 3

if command -sq starship
  # https://starship.rs/#fish
  starship init fish | source
end
