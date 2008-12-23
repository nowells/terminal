# If not running interactively, don't do anything
[ -z "$PS1" ] && return

case `uname` in
    IRIX64)
        SYS_BASE=sgi
        ;;
    SunOS)
        SYS_BASE=sun
        ;;
    Linux)
        SYS_BASE=linux
        ;;
    Darwin)
        SYS_BASE=mac
        ;;
    *)
        SYS_BASE=unknown
        ;;
esac

# Source global definitions
if [ -f /etc/bashrc ]; then
    # Global bashrc
	. /etc/bashrc
fi

if [ -r /etc/bash_completion ]; then
   # Source completion code.
   . /etc/bash_completion
fi

if [ -f ~/.bashrc.d/visual ]; then
    # Visual styles
    . ~/.bashrc.d/visual
fi

if [ -f ~/.bashrc.d/exports ]; then
    # Exports
    . ~/.bashrc.d/exports
fi

if [ -f ~/.bashrc.d/aliases ]; then
    # Exports
    . ~/.bashrc.d/aliases
fi

if [ -f ~/.bashrc.d/development ]; then
    # Exports
    . ~/.bashrc.d/development
fi

if [ -f ~/.bashrc.d/functions ]; then
    # Exports
    . ~/.bashrc.d/functions
fi

if [ -f ~/.bashrc.d/local ]; then
    # Exports
    . ~/.bashrc.d/local
fi

# User specific aliases and functions
umask 002 # allow user + group to write, no other

# append to bash_history if shell quites
shopt -s histappend

# check the window size after each command and, if necessary, update the values of LINES and COLUMNS.
shopt -s checkwinsize

# prevent overwriting files with cat
set -o noclobber

# Stop ctrl+d from logging me out
set -o ignoreeof

# disable Ctrl-S and Ctrl-Q, which suck!
stty -ixon

stty erase ^H
stty ek

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Security: close root shells after n seconds of inactivity
[ "$UID" = 0 ] && export TMOUT=180
