#!/bin/bash

sudo echo "  Authentication succeeded"

# if you want to execute this installer as a root too,
# please remove this function.
if [ "$EUID" = 0 ]; then
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   echo '     !!    you are executing as root!     !!'
   echo '     !!  do not use sudo, and try again!  !!'
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   echo 'exit at [1]'
   exit
fi

# check location of dotfiles repository
if [[ $(pwd) = $HOME/repositories/matoruru/dotfiles ]]; then
   echo "location of this repository is correct!"
else
   echo "location of this repository is not correct..."
   echo "move this repository, to ~/repositories/matoruru/"
   echo 'exit at [2]'
   exit
fi


function check_files_existance {
   # if the file is exist already, create _old file
   if [ -f $1 ]; then
      mv $1 $1.old
   fi
}

function create_link {
   # create symbolic link like $2 -> $1
   sudo ln -sr $1 $2
}
function create_link_sudo {
   # create symbolic link like $2 -> $1
   sudo ln -sr $1 $2
}


function installer_portage_files {
   # install portage files,
   # include make.conf, package.use
   echo "[ install portage files ]"
   sudo rm                              /etc/portage/make.conf
   sudo rm -rf                          /etc/portage/package.use
   create_link_sudo portage/make.conf   /etc/portage/
   create_link_sudo portage/package.use /etc/portage/
   sudo emerge-webrsync
   sudo emerge --sync && sudo emerge -uDN @world
}
installer_portage_files


function installer_shell_files {
   # install rc files
   echo "[ install rc files ]"
   check_files_existance $HOME/.bash_profile
   create_link                 .bash_profile ~/

   check_files_existance $HOME/.bashrc
   create_link                 .bashrc ~/

   FISHDIR="$HOME/.config/fish"
   if [ -d $FISHDIR ]; then
      check_files_existance $FISHDIR/config.fish
   else
      mkdir -p $FISHDIR
   fi
   if [ -f $HOME/.fishrc ]; then
      rm ~/.fishrc
   fi
   create_link .fishrc $FISHDIR/config.fish
   create_link         $FISHDIR/config.fish ~/.fishrc
}
installer_shell_files


function installer_vimrc {
   # install vimrc
   echo "[ install vimrc ]"
   check_files_existance $HOME/.vimrc
   create_link                 .vimrc ~/
}
installer_vimrc


function installer_xmonad {
   # install xmonad
   echo "[ install xmonad ]"
   check_files_existance $HOME/.xinitrc
   create_link                 .xinitrc ~/

   if [ -d $HOME/.xmonad ]; then
      check_files_existance $HOME/.xmonad/xmonad.hs
   else
      mkdir              ~/.xmonad
   fi
   create_link xmonad.hs ~/.xmonad/

   check_files_existance $HOME/.Xresources
   create_link                 .Xresources ~/
}
installer_xmonad
