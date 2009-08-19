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

BASHRC_CONFIG_DIR=~/
if [[ $__REMOTE_TERMINAL_SCRIPT_DIR ]]; then
    BASHRC_CONFIG_DIR=$__REMOTE_TERMINAL_SCRIPT_DIR
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
    # Global bashrc
	. /etc/bashrc
fi

if [ -r /etc/bash_completion ]; then
   # Source completion code.
   . /etc/bash_completion
fi

if [ -r $BASHRC_CONFIG_DIR/.bashrc.d/completions/git-completion.bash ]; then
   # Source completion code.
   . $BASHRC_CONFIG_DIR/.bashrc.d/completions/git-completion.bash
fi

if [ -r $BASHRC_CONFIG_DIR/.bashrc.d/completions/hg-completion.bash ]; then
   # Source completion code.
   . $BASHRC_CONFIG_DIR/.bashrc.d/completions/hg-completion.bash
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/visual ]; then
    # Visual styles
    . $BASHRC_CONFIG_DIR/.bashrc.d/visual
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/exports ]; then
    # Exports
    . $BASHRC_CONFIG_DIR/.bashrc.d/exports
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/aliases ]; then
    # Aliases
    . $BASHRC_CONFIG_DIR/.bashrc.d/aliases
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/development ]; then
    # Development
    . $BASHRC_CONFIG_DIR/.bashrc.d/development
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/functions ]; then
    # Functions
    . $BASHRC_CONFIG_DIR/.bashrc.d/functions
fi

if [ -f $BASHRC_CONFIG_DIR/.bashrc.d/local ]; then
    # Local overrides
    . $BASHRC_CONFIG_DIR/.bashrc.d/local
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

export JPY=~/lib/j2/j.py
. ~/lib/j2/j.sh