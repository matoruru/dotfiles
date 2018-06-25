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


function installer_portage_files {
   # install portage files,
   # include make.conf, package.use
   echo "[ install portage files ]"
   sudo rm     /etc/portage/make.conf
   sudo rm -rf /etc/portage/package.use
   sudo ln -sr portage/make.conf   /etc/portage/
   sudo ln -sr portage/package.use /etc/portage/
   sudo emerge-webrsync
   sudo emerge --sync && sudo emerge -uDN @world
}
installer_portage_files


function installer_shell_files {
   # install rc files
   echo "[ install rc files ]"
   check_files_existance $HOME/.bash_profile
   ln -sr .bash_profile ~/

   check_files_existance $HOME/.bashrc
   ln -sr .bashrc ~/

   FISHDIR="$HOME/.config/fish"
   if [ -d $FISHDIR ]; then
      check_files_existance $FISHDIR/config.fish
   else
      mkdir -p $FISHDIR
   fi
   if [ -f $HOME/.fishrc ]; then
      rm ~/.fishrc
   fi
   ln -sr .fishrc $FISHDIR/config.fish
   ln -sr         $FISHDIR/config.fish ~/.fishrc
}
installer_shell_files


function installer_vimrc {
   # install vimrc
   echo "[ install vimrc ]"
   check_files_existance $HOME/.vimrc
   ln -sr .vimrc ~/
}
installer_vimrc


function installer_xmonad {
   # install xmonad
   echo "[ install xmonad ]"
   check_files_existance $HOME/.xinitrc
   ln -sr .xinitrc ~/

   if [ -d $HOME/.xmonad ]; then
      check_files_existance $HOME/.xmonad/xmonad.hs
   else
      mkdir ~/.xmonad
   fi
   ln -sr xmonad.hs ~/.xmonad/

   check_files_existance $HOME/.Xresources
   ln -sr .Xresources ~/
}
installer_xmonad
