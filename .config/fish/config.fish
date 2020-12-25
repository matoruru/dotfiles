#!/bin/fish

set fish_greeting

set -x PATH \
  ~/.npm-global/bin \
  ~/.nodebrew/current/bin \
  ~/.yarn-global/bin \
  ~/.local/bin \
  ~/.ghcup/bin \
  $PATH

# Set alias
alias nvimtutor="nvim -c Tutor"

# Set tab step
tabs 3

# https://github.com/starship/starship#fish
starship init fish | source
