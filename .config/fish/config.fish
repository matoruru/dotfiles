#!/bin/fish

set fish_greeting

# To use 'npm i -g' wituout sudo
if test -d ~/.npm-global
   set -x PATH ~/.npm-global/bin $PATH
end

if test -d ~/.nodebrew
   set -x PATH ~/.nodebrew/current/bin $PATH
end

if test -d ~/.yarn-global
   set -x PATH ~/.yarn-global/bin $PATH
end

# Set alias
alias nvimtutor="nvim -c Tutor"

# Set tab step
tabs 3

# https://github.com/starship/starship#fish
starship init fish | source
