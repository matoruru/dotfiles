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

source /opt/miniconda3/etc/fish/conf.d/conda.fish

# Set default editor and options
set default_editor 'vim'
set editor_options '-p'
alias  e="$default_editor $editor_options"

# Set alias
alias vim="nvim"
alias mocp='mocp -T mytheme'
alias showtodo="fish ~/repositories/matoruru/gentoo-tools/showtodo.fish"
alias edittodo="nvim ~/todolist.txt;showtodo"
alias su="sudo su - -m"
alias scrot="scrot -q 100"
alias nb="nodebrew"
alias dvim="nvim -u ~/.vim/myplugin/essential.vim"

# funny commnad
alias えぃｔ="exit"
alias ゔぃｍ="vim"

# Set tab step
tabs 3

# Set theme
set theme 'gruvbox'

if      test $theme = 'solarized'
   set yimmy_solarized true
else if test $theme = 'gruvbox'
   set yimmy_solarized false
   theme_gruvbox dark
end

. ~/repositories/theme-yimmy/fish_prompt.fish

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
