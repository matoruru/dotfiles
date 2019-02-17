# /etc/skel/.bash_profile

if [[ -d ~/.config/pulse ]]; then
   echo "remove all file in ~/.config/pulse/"
   rm ~/.config/pulse/*
fi

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
   if type startx; then
      exec startx
   else
      echo ".bash_profile : startx does not exist"
   fi
fi

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]]; then
	. ~/.bashrc
fi
