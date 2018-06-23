#!/bin/bash

# if you want to execute this installer as a root too,
# please remove this statements.
if [[ "$EUID" = 0 ]]; then
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   echo '     !!    you are executing as root!     !!'
   echo '     !!  do not use sudo, and try again!  !!'
   echo '     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   exit
fi

# install rc files
cp .bash_profile ~/
cp .bashrc       ~/
mkdir -p      ~/.config/fish
cp    .fishrc ~/.config/fish/config.fish
ln    -s      ~/.config/fish/config.fish ~/.fishrc

# install xmonad
cp    /etc/X11/xinit/xinitrc   ~/.xinitrc
cat  .xinitrc                > ~/.xinitrc
mkdir           ~/.xmonad
cp    xmonad.hs ~/.xmonad/


# install urxvtc (rxvt-unicode as a daemon)
cp .Xresources ~/
