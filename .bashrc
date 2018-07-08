# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

function showtodo {
   # if you want do, please make "~/todolist.txt"!
   echo
   echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
   echo "=-=-=-=-=-=-=-=-=-=-=  My TODO  =-=-=-=-=-=-=-=-=-=-="
   echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
   echo
   if [[ -f ~/todolist.txt ]]; then
      cat  ~/todolist.txt
   else
      echo "  - create ~/todolist.txt to show todolist"
   fi
   echo
   echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
   echo
}

# Put your fun stuff here.

tabs 3

alias ll="ls -alF"
alias vim='vim -p'
alias edittodo="vim ~/todolist.txt"

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then :; else showtodo; fi

# execute this after loading bash settings
ISFISHON=true
if [[ "$ISFISHON" = "true" ]]; then
   exec fish
fi
