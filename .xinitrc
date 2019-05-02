#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/Xresources
sysmodmap=/etc/X11/xinit/Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then


    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then


    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -n "`/etc/X11/chooser.sh`" ]; then
 command="`/etc/X11/chooser.sh`"
else
 failsafe="yes"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?* ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

if [ -n "$failsafe" ]; then
# for mozc
 export GTK_IM_MODULE=fcitx
 export QT_IM_MODULE=fcitx
 export XMODIFIERS="@im=fcitx"
 fcitx &

# for android-studio
 export _JAVA_AWT_WM_NONREPARENTING=1
 export ANDROID_HOME=$HOME/AndroidTools
 export ANDROID_SDK_ROOT=$ANDROID_HOME/sdk

# for authenticate on sfdx
 eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
 export SSH_AUTH_SOCK

# set wallpaper by using feh
# start quickly if wallpaper doesn't set yet => append &

# start urxvt as a daemon, after this, use urxvtc to start urxvt
 urxvtd --quiet --fork --opendisplay &

 compton -b

 if type amixer; then
    amixer set Master off &
    echo ".bash_profile : Master volume was turned off."
 fi

 bash ~/.config/polybar/launch.sh &

 xbacklight -set 46 -time 1 &

 xset r rate 240 30 &

 exec xmonad

else
 exec $command
fi
