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

# move to the home directory first
cd

# check a connection to internet
ping www.google.com -i 0.2 -c 5 || { echo "Connection is not established..."; exit; }

(
   mkdir -p ~/repositories/matoruru
   cd       ~/repositories/matoruru

   git clone https://github.com/matoruru/arch-tools.git &
   git clone https://github.com/matoruru/polybar-adapta-theme.git &
   git clone https://github.com/matoruru/dotfiles.git &
   git clone https://github.com/matoruru/.xmonad.git ~/.xmonad &

   wait
)

if [ ! -d ~/repositories/matoruru/arch-tools ]; then
  echo "matoruru/arch-tools does not exist, try again."
  exit
fi

if [ ! -d ~/repositories/matoruru/polybar-adapta-theme ]; then
  echo "matoruru/polybar-adapta-theme does not exist, try again."
  exit
fi

if [ ! -d ~/repositories/matoruru/dotfiles ]; then
  echo "matoruru/dotfiles does not exist, try again."
  exit
fi

if [ ! -d ~/.xmonad ]; then
  echo "matoruru/.xmonad does not exist, try again."
  exit
fi

# dotfiles
(
   mkdir -p ~/.config

   cd ~/repositories/matoruru/dotfiles

   # install rc files
   echo "[ install rc files ]"
   ln -srf .bash_profile ~/
   ln -srf .bashrc ~/
   mkdir -p                         ~/.config/fish
   ln -srf .config/fish/config.fish ~/.config/fish/
   ln -srf .config/fish/config.fish ~/.fishrc
   ln -srf .npmrc ~/

   # install docker config
   ln -srf ./.docker ~/

   # install vimrc
   echo "[ install vimrc  ]"
   mkdir -p ~/.vim/view
   mkdir -p ~/.local/share/nvim/view

   mkdir -p ~/.config/nvim
   ln -srf .nvimrc ~/.nvimrc

   ln -srf .config/nvim/coc-settings.json ~/.config/nvim/
   ln -srf .config/nvim/init.vim ~/.config/nvim/

   # install .gitconfig
   ln -srf .gitconfig ~/


   # install X files
   ln -srf .xinitrc ~/
   ln -srf .Xresources ~/
   ln -srf .Xmodmap-internal ~/


   # install picom
   echo "[ install picom ]"
   ln -srf .config/picom.conf ~/.config/


   # install rofi
   echo "[ install rofi ]"
   mkdir -p ~/.config/rofi
   ln -srf    .config/rofi/config ~/.config/rofi/

   # install alacritty
   echo "[ install alacritty ]"
   mkdir -p ~/.config/alacritty
   ln -srf    .config/alacritty/alacritty.yml ~/.config/alacritty/

   # install starship
   echo "[ install starship ]"
   ln -srf .config/starship.toml ~/.config/

   # install gtk-2.0
   echo "[ install gtk-2.0  ]"
   ln -srf .gtkrc-2.0 ~/

   # install gtk-3.0
   echo "[ install gtk-3.0  ]"
   mkdir -p ~/.config/gtk-3.0
   ln -srf    .config/gtk-3.0/settings.ini ~/.config/gtk-3.0/

   # install user locale file (for fcitx)
   echo "[ install locale.conf ]"
   ln -srf .config/locale.conf ~/.config/

   # install polybar
   echo "[ install polybar ]"
   ln -srf ../polybar-adapta-theme/polybar ~/.config/

   # install inkscape
   echo "[ install inkscape ]"
   mkdir -p ~/.config/inkscape
   cp         .config/inkscape/preferences.xml ~/.config/inkscape/
)

(
   # launch arch-tools installer
   cd ~/repositories/matoruru/arch-tools
   bash install.sh
)
