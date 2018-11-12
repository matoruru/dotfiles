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

# Put your fun stuff here.
alias ll="ls -alF"
alias vim='vim -p'
alias vi='vim'
alias mocp='mocp -T mytheme'
alias showtodo="fish ~/repositories/matoruru/gentoo-tools/showtodo.fish"
alias edittodo="vim ~/todolist.txt;showtodo"

# what want to do when bash is executed
tabs 3

if [[ $(tty) = '/dev/pts/0' ]]; then
   showtodo
fi

#rm ~/.serverauth.* 1> /dev/null 2>&1

# execute this after loading bash settings
ISFISHON=true
if [[ "$ISFISHON" = "true" ]]  && [[ -f /bin/fish ]]; then
   exec fish
fi
