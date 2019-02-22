#!/bin/bash

sudo echo "  Authentication succeeded"

# if you want to execute this installer as a root too,
# please remove this function.
if [[ "$EUID" = 0 ]]; then
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

# check connection to internet
ping www.google.com -i 0.2 -c 5 || exit

(
   # clone my repositories
   cd ~/repositories/matoruru
   git clone git@github.com:matoruru/gentoo-tools.git
   git clone git@github.com:matoruru/omoshiro-tools.git
   git clone git@github.com:matoruru/xmonad.hs.git
)

# install rc files
echo "[ install rc files ]"
ln -srb .bash_profile ~/
ln -srb .bashrc ~/
ln -srb .config/fish/config.fish ~/.config/fish/
ln -srb .config/fish/config.fish ~/.fishrc


# install vimrc
echo "[ install vimrc  ]"
ln -srb .vimrc  ~/
mkdir -p ~/.vim/view


# install X files
ln -srb .xinitrc ~/
ln -srb .Xresources ~/
ln -srb .Xmodmap ~/


# install xmonad
echo "[ install xmonad ]"
mkdir                     ~/.xmonad
ln -srb .xmonad/xmonad.hs ~/.xmonad/


# install xmobar
echo "[ install xmobar ]"
ln -srb .xmobarrc ~/


# install feh
cp .fehbg  ~/
chmod 754  ~/.fehbg


# install compton
echo "[ install compton  ]"
ln -srb .compton.conf ~/


# install rofi
echo "[ install rofi ]"
mkdir -p ~/.config/rofi
ln -srb    .config/rofi/config ~/.config/rofi/


# install moc
echo "[ install moc ]"
mkdir               ~/.moc
ln -srb .moc/themes ~/.moc/
chmod 755 ~/.moc/themes/changetheme.sh


# install ctags
echo "[ install ctags  ]"
ln -srb ./.ctags ~/


# install qutebrowser
echo "[ install qutebrowser ]"
mkdir -p                              ~/.config/qutebrowser
ln -srb .config/qutebrowser/config.py ~/.config/qutebrowser/


# install kitty
echo "[ install kitty ]"
mkdir -p ~/.config/kitty
ln -srb    .config/kitty/kitty.conf ~/.config/kitty/


# install gtk-3.0
echo "[ install gtk-3.0  ]"
mkdir -p ~/.config/gtk-3.0
ln -srb    .config/gtk-3.0/settings.ini ~/.config/gtk-3.0/


echo "execute gentoo-tools' install.sh!"
