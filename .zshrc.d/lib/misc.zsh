## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## file rename magick
autoload -U zmv
bindkey "^[m" copy-prev-shell-word

## jobs
setopt long_list_jobs

## pager
export PAGER='less -R'
export LC_CTYPE=en_US.UTF-8

## pretty man pages
function pman() {
    man $1 -t | open -f -a Preview
}

# Stop ctrl+d from logging me out
set -o ignoreeof

# disable Ctrl-S and Ctrl-Q, which suck!
stty -ixon

#stty erase ^H
stty ek

# User specific aliases and functions
umask 002 # allow user + group to write, no other
