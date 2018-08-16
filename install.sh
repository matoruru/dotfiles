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


function check_files_existance {
   # if the file is exist already, create _old file
   if [[ -f $1 ]]; then
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

# check connection to internet
ping www.google.com -c 3 || exit

function installer_portage_files {
   # install portage files,
   # include make.conf, package.use
   echo "[ install portage files ]"
   sudo rm                                /etc/portage/make.conf
   create_link_sudo portage/make.conf     /etc/portage/
   if [[ -f /etc/portage/package.use/zz-autounmask ]]; then
      sudo mv /etc/portage/package.use/zz-autounmask /etc/portage/
   fi
   sudo rm -rf                            /etc/portage/package.use
   create_link_sudo portage/package.use   /etc/portage/
   sudo mv /etc/portage/zz-autounmask     /etc/portage/package.use/
}
installer_portage_files


function installer_shell_files {
   # install rc files
   echo "[ install rc files ]"
   check_files_existance $HOME/.bash_profile
   create_link                 .bash_profile ~/

   check_files_existance $HOME/.bashrc
   create_link                 .bashrc ~/

   cp todolist.txt ~/

   FISHDIR="$HOME/.config/fish"
   if [[ -d $FISHDIR ]]; then
      check_files_existance $FISHDIR/config.fish
   else
      mkdir -p $FISHDIR
   fi
   if [[ -f $HOME/.fishrc ]]; then
      rm ~/.fishrc
   fi
   create_link .fishrc $FISHDIR/config.fish
   create_link         $FISHDIR/config.fish ~/.fishrc
}
installer_shell_files


function installer_vimrc {
   # install vimrc
   echo "[ install vimrc  ]"
   check_files_existance $HOME/.vimrc
   create_link                 .vimrc  ~/

   # install gvimrc
   echo "[ install gvimrc ]"
   check_files_existance $HOME/.gvimrc
   create_link                 .gvimrc ~/

   mkdir -p ~/.vim/view

   sudo cp .vimrc-root /root/.vimrc
}
installer_vimrc


function installer_xmonad {
   # install xmonad
   echo "[ install xmonad ]"
   check_files_existance $HOME/.xinitrc
   create_link                 .xinitrc ~/

   if [[ -d $HOME/.xmonad ]]; then
      check_files_existance $HOME/.xmonad/xmonad.hs
   else
      mkdir              ~/.xmonad
   fi
   create_link xmonad.hs ~/.xmonad/

   cp .fehbg  ~/
   chmod 754  ~/.fehbg

   check_files_existance $HOME/.Xresources
   create_link                 .Xresources ~/
}
installer_xmonad

function installer_compton {
   # install compton
   echo "[ install compton  ]"
   check_files_existance $HOME/.compton.conf
   create_link                 .compton.conf ~/
}
installer_compton

function installer_xmobar {
   # install xmobar
   echo "[ install xmobar ]"
   check_files_existance $HOME/.xmobarrc
   create_link                 .xmobarrc ~/
}
installer_xmobar

function installer_rofi {
   # install rofi
   echo "[ install rofi ]"
   if [[ -d $HOME/.config/rofi ]]; then
      check_files_existance $HOME/.config/rofi/config
   else
      mkdir -p ~/.config/rofi
   fi
   create_link config ~/.config/rofi/
}
installer_rofi

function clone_my_gentoo_tools {
   # clone gentoo-tools
   cd ~/repositories/matoruru
   git clone https://github.com/matoruru/gentoo-tools.git
   cd dotfiles
}
clone_my_gentoo_tools

function installer_moc {
   # install moc
   echo "[ install moc ]"
   mkdir                   ~/.moc
   create_link .moc/themes ~/.moc/
   chmod 755               ~/.moc/themes/changetheme.sh
}
installer_moc

function installer_ctags  {
   # install ctags
   echo "[ install ctags  ]"
   check_files_existance $HOME/.ctags
   create_link               ./.ctags ~/
}
installer_ctags

function installer_xmodmap  {
   # install xmodmap
   echo "[ install xmodmap  ]"
   check_files_existance $HOME/.Xmodmap
   create_link               ./.Xmodmap ~/
}
installer_xmodmap
