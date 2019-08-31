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

# check connection to internet
ping www.google.com -i 0.2 -c 5 || exit

(
   mkdir -p ~/repositories/matoruru
   cd       ~/repositories/matoruru

   git clone git@github.com:matoruru/gentoo-tools.git &
   git clone git@github.com:matoruru/arch-tools.git &
   git clone git@github.com:matoruru/polybar-adapta-theme.git &
   git clone git@github.com:matoruru/imgs.git &
   git clone git@github.com:matoruru/omoshiro-tools.git &
   git clone git@github.com:matoruru/vimtutor-ja &

   git clone git@github.com:matoruru/dotfiles.git
)

# move to the directory
cd ~/repositories/matoruru/dotfiles

# install rc files
echo "[ install rc files ]"
ln -srb .bash_profile ~/
ln -srb .bashrc ~/
mkdir -p                         ~/.config/fish
ln -srb .config/fish/config.fish ~/.config/fish/
ln -srb .config/fish/config.fish ~/.fishrc
ln -srb .npmrc ~/


# install vimrc
echo "[ install vimrc  ]"
#ln -srb .vimrc   ~/
mkdir -p ~/.vim/view
mkdir -p ~/.local/share/nvim/view

mkdir -p ~/.config/nvim
echo "source ~/.nvimrc" > ~/.config/nvim/init.vim
ln -srb .nvimrc ~/.nvimrc

ln -srb .config/nvim/coc-settings.json ~/.config/nvim/

sudo mkdir -p /root/.config/nvim
echo "source ~/.nvimrc" | sudo tee /root/.config/nvim/init.vim > /dev/null
sudo ln -srb .nvimrc-root /root/.nvimrc


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


# install kitty
echo "[ install kitty ]"
mkdir -p ~/.config/kitty
ln -srb    .config/kitty/kitty.conf ~/.config/kitty/

# install gtk-2.0
echo "[ install gtk-2.0  ]"
ln -srb .gtkrc-2.0 ~/

# install gtk-3.0
echo "[ install gtk-3.0  ]"
mkdir -p ~/.config/gtk-3.0
ln -srb    .config/gtk-3.0/settings.ini ~/.config/gtk-3.0/

# install user locale file (for fcitx)
echo "[ install locale.conf ]"
ln -srb .config/locale.conf ~/.config/

# install polybar
echo "[ install polybar ]"
ln -srb ../polybar-adapta-theme/polybar ~/.config/
