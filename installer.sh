#!/bin/bash

function check_who_execute {
   # if you want to execute this installer as a root too,
   # please remove this function.
   if [ "$EUID" = 0 ]; then
      echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      echo '     !!    you are executing as root!     !!'
      echo '     !!  do not use sudo, and try again!  !!'
      echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      exit
   fi
}
check_who_execute


function create_oldfile {
   mv $1 $1.old
}


function installer_rc_files {
   # install rc files
   echo "[ install rc files ]"
   if [ -f $HOME/.bash_profile ]; then
      create_oldfile $HOME/.bash_profile
   fi
   ln -sr .bash_profile ~/

   if [ -f $HOME/.bashrc ]; then
      create_oldfile $HOME/.bashrc
   fi
   ln -sr .bashrc ~/

   FISHDIR="$HOME/.config/fish"
   if [ -d $FISHDIR ]; then
      if [ -f $FISHDIR/config.fish ]; then
         create_oldfile $FISHDIR/config.fish
      fi
   else
      mkdir -p $FISHDIR
   fi
   ln -sr .fishrc $FISHDIR/config.fish
   ln -sr         $FISHDIR/config.fish ~/.fishrc
}
installer_rc_files


function installer_vimrc {
   # install vimrc
   echo "[ install vimrc ]"
   if [ -f $HOME/.vimrc ]; then
      create_oldfile $HOME/.vimrc
   fi
   ln -sr .vimrc ~/
}
installer_vimrc


function installer_xmonad {
   # install xmonad
   echo "[ install xmonad ]"
   if [ -f $HOME/.xinitrc ]; then
      create_oldfile $HOME/.xinitrc
   fi
   ln -sr .xinitrc ~/

   if [ -d $HOME/.xmonad ]; then
      if [ -f $HOME/.xmonad/xmonad.hs ]; then
         create_oldfile $HOME/.xmonad/xmonad.hs
      fi
   else
      mkdir ~/.xmonad
   fi
   ln -sr xmonad.hs ~/.xmonad/

   if [ -f $HOME/.Xresources ]; then
      create_oldfile $HOME/.Xresources
   fi
   ln -sr .Xresources ~/
}
installer_xmonad
