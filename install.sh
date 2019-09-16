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

# check a connection to internet
ping www.google.com -i 0.2 -c 5 || exit

# check if ssh works
ssh -o "StrictHostKeyChecking no" -T git@github.com || test $? = 255 && echo "fail"

(
   mkdir -p ~/repositories/matoruru
   cd       ~/repositories/matoruru

   git clone git@github.com:matoruru/arch-tools.git &
   git clone git@github.com:matoruru/dotfiles.git
)

if [ ! -d ~/repositories/matoruru/arch-tools ]; then
  echo "matoruru/arch-tools does not exist, try again."
  exit
fi

if [ ! -d ~/repositories/matoruru/dotfiles ]; then
  echo "matoruru/dotfiles does not exist, try again."
  exit
fi

(
   cd ~/repositories/matoruru/dotfiles

   # install rc files
   echo "[ install rc files ]"
   ln -sr .bash_profile ~/
   ln -sr .bashrc ~/
   mkdir -p                         ~/.config/fish
   ln -sr .config/fish/config.fish ~/.config/fish/
   ln -sr .config/fish/config.fish ~/.fishrc
   ln -sr .npmrc ~/


   # install vimrc
   echo "[ install vimrc  ]"
   #ln -sr .vimrc   ~/
   mkdir -p ~/.vim/view
   mkdir -p ~/.local/share/nvim/view

   mkdir -p ~/.config/nvim
   echo "source ~/.nvimrc" > ~/.config/nvim/init.vim
   ln -sr .nvimrc ~/.nvimrc

   ln -sr .config/nvim/coc-settings.json ~/.config/nvim/

   sudo mkdir -p /root/.config/nvim
   echo "source ~/.nvimrc" | sudo tee /root/.config/nvim/init.vim > /dev/null
   sudo ln -sr .nvimrc-root /root/.nvimrc


   # install X files
   ln -sr .xinitrc ~/
   ln -sr .Xresources ~/
   ln -sr .Xmodmap ~/


   # install xmonad
   echo "[ install xmonad ]"
   mkdir                     ~/.xmonad
   ln -sr .xmonad/xmonad.hs ~/.xmonad/


   # install xmobar
   echo "[ install xmobar ]"
   ln -sr .xmobarrc ~/


   # install compton
   echo "[ install compton  ]"
   ln -sr .compton.conf ~/


   # install rofi
   echo "[ install rofi ]"
   mkdir -p ~/.config/rofi
   ln -sr    .config/rofi/config ~/.config/rofi/


   # install moc
   echo "[ install moc ]"
   mkdir               ~/.moc
   ln -sr .moc/themes ~/.moc/
   chmod 755 ~/.moc/themes/changetheme.sh


   # install ctags
   echo "[ install ctags  ]"
   ln -sr ./.ctags ~/


   # install kitty
   echo "[ install kitty ]"
   mkdir -p ~/.config/kitty
   ln -sr    .config/kitty/kitty.conf ~/.config/kitty/

   # install gtk-2.0
   echo "[ install gtk-2.0  ]"
   ln -sr .gtkrc-2.0 ~/

   # install gtk-3.0
   echo "[ install gtk-3.0  ]"
   mkdir -p ~/.config/gtk-3.0
   ln -sr    .config/gtk-3.0/settings.ini ~/.config/gtk-3.0/

   # install user locale file (for fcitx)
   echo "[ install locale.conf ]"
   ln -sr .config/locale.conf ~/.config/

   # install polybar
   echo "[ install polybar ]"
   ln -sr ../polybar-adapta-theme/polybar ~/.config/
)

(
   # launch arch-tools installer
   cd ~/repositories/matoruru/arch-tools
   bash install.sh
)
