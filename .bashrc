# .bashrc

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

# User specific aliases and functions
umask 002 # allow user + group to write, no other

stty erase ^H
stty ek
# append to bash_history if shell quites
shopt -s histappend
