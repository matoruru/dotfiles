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


   # install vimrc
   echo "[ install vimrc  ]"
   #ln -srf .vimrc   ~/
   mkdir -p ~/.vim/view
   mkdir -p ~/.local/share/nvim/view

   mkdir -p ~/.config/nvim
   echo "source ~/.nvimrc" > ~/.config/nvim/init.vim
   ln -srf .nvimrc ~/.nvimrc

   ln -srf .config/nvim/coc-settings.json ~/.config/nvim/

   sudo mkdir -p /root/.config/nvim
   echo "source ~/.nvimrc" | sudo tee /root/.config/nvim/init.vim > /dev/null
   sudo ln -srf .nvimrc-root /root/.nvimrc


   # install X files
   ln -srf .xinitrc ~/
   ln -srf .Xresources ~/
   ln -srf .Xmodmap-internal ~/


   # install xmonad
   echo "[ install xmonad ]"
   mkdir                     ~/.xmonad
   ln -srf .xmonad/xmonad.hs ~/.xmonad/


   # install xmobar
   echo "[ install xmobar ]"
   ln -srf .xmobarrc ~/


   # install picom
   echo "[ install picom ]"
   ln -srf .config/picom.conf ~/.config/


   # install rofi
   echo "[ install rofi ]"
   mkdir -p ~/.config/rofi
   ln -srf    .config/rofi/config ~/.config/rofi/


   # install moc
   echo "[ install moc ]"
   mkdir               ~/.moc
   ln -srf .moc/themes ~/.moc/
   chmod 755 ~/.moc/themes/changetheme.sh


   # install ctags
   echo "[ install ctags  ]"
   ln -srf ./.ctags ~/


   # install kitty
   echo "[ install kitty ]"
   mkdir -p ~/.config/kitty
   ln -srf    .config/kitty/kitty.conf ~/.config/kitty/

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
   #bash install.sh
)
