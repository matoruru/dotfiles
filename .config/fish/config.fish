#!/bin/fish

# To remove the message at fish login
set fish_greeting

# To use 'npm i -g' wituout sudo
if test -d ~/.npm-global
   set -x PATH ~/.npm-global/bin $PATH
end

if test -d ~/.nodebrew
   set -x PATH $HOME/.nodebrew/current/bin $PATH
end

if test -d ~/.yarn-global
   set -x PATH /home/matoruru/.yarn-global/bin $PATH
end

#source /opt/miniconda3/etc/fish/conf.d/conda.fish

# Set alias
alias n='nvim'
alias mocp='mocp -T mytheme'
alias showtodo="fish ~/repositories/matoruru/gentoo-tools/showtodo.fish"
alias edittodo="nvim ~/todolist.txt;showtodo"
alias su="sudo su - -m"
alias scrot="scrot -q 100"
alias nb="nodebrew"
alias dvim="nvim -u ~/.vim/myplugin/essential.vim"

alias pulp="yarn run pulp"
alias purs="yarn run purs"
alias spago="yarn run spago"
alias bower="yarn run bower"
alias parcel="yarn run parcel"
alias psc-package="yarn run psc-package"

# funny commnad
alias えぃｔ="exit"
alias ゔぃｍ="vim"
alias んゔぃｍ="nvim"

# Set tab step
tabs 3

# Base16 Shell
if status --is-interactive
   set BASE16_SHELL "$HOME/.config/base16-shell/"
   source "$BASE16_SHELL/profile_helper.fish"
end

# Change greeting
set -g simple_ass_prompt_greeting "Hello matoruru."

# switch apatche server on/off
function httpon
   sudo /etc/init.d/apache2 start
end

function httpoff
   sudo /etc/init.d/apache2 stop
end

# switch docker on/off
function dockeron
   sudo rc-service docker start
end

function dockeroff
   sudo rc-service docker stop
end
