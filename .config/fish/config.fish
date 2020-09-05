#!/bin/fish

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
alias su="sudo su - -m"

# Set tab step
tabs 3

# Base16 Shell
if status --is-interactive
   set BASE16_SHELL "$HOME/.config/base16-shell/"
   source "$BASE16_SHELL/profile_helper.fish"
end

# Change greeting
set -g simple_ass_prompt_greeting "Welcome!"
