#!/bin/bash

# if you want to execute this installer as a root too,
# please remove this statements.
if [ "$EUID" = 0 ]; then
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   echo '     !!    you are executing as root!     !!'
   echo '     !!  do not use sudo, and try again!  !!'
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   exit
fi

function create_oldfile {
   mv $1 $1.old
}

# install rc files
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

# install vimrc
cp .vimrc ~/

# install xmonad
cp    /etc/X11/xinit/xinitrc   ~/.xinitrc
cat  .xinitrc                > ~/.xinitrc
mkdir           ~/.xmonad
cp    xmonad.hs ~/.xmonad/

# install urxvtc (rxvt-unicode as a daemon)
cp .Xresources ~/
