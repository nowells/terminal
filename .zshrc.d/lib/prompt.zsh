precmd() {
    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))


    ###
    # Truncate the path if it's too long.

    PR_FILLBAR=""
    PR_PWDLEN=""

    local promptsize=${#${:-(%n@%m)-----------------------}}
    local pwdsize=${#${(%):-%~}}

    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	   ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
        PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi

    activate_ve
}

chpwd() {
    settitle
    activate_ve
}

activate_ve() {
    local DIR=$PWD
    local LASTDIR=""
    until [[ $LASTDIR = $DIR ]]; do
        if [ -f $DIR/ve/bin/activate ]; then
            export VIRTUAL_ENV=$DIR/ve
            return
        fi
        LASTDIR=$DIR
        DIR=$(dirname $DIR)
    done
    unset VIRTUAL_ENV
}

settitle() {
    if [[ "$TERM" == "screen" ]]; then
       local CMD='shell'
       local DIR=$PWD
       local LASTDIR=""
       until [[ $LASTDIR = $DIR ]]; do
           if [ -f $DIR/ve/bin/activate ]; then
               CMD=$(basename $DIR)
               break
           fi
           if [ -f $DIR/.git/HEAD ]; then
               CMD=$(basename $DIR)
               break
           fi
           if [ -f $DIR/.hg/branch ]; then
               CMD=$(basename $DIR)
               break
           fi
           LASTDIR=$DIR
           DIR=$(dirname $DIR)
       done
       echo -ne "\ek$CMD\e\\"
    fi
}

setopt extended_glob

vcsprompt() {
    local DIR=$PWD
    local LASTDIR=''
    local REF=''
    local VCS_ICON=''
    local BRANCH_COLOR=$PR_GREEN
    local OUTPUT=''
    until [[ $LASTDIR = $DIR ]]; do
        if [ -f $DIR/.git/HEAD ]; then
            REF=$(git --git-dir $DIR/.git symbolic-ref HEAD 2> /dev/null | sed 's/refs\/heads\///g' 2> /dev/null)
            VCS_ICON='git'
            break
        fi
        if [ -f $DIR/.hg/branch ]; then
            REF=$(cat $DIR/.hg/branch 2> /dev/null)
            VCS_ICON='hg'
            break
        fi
        LASTDIR=$DIR
        DIR=$(dirname $DIR)
    done

    if [[ $REF = 'master' ]]; then
        BRANCH_COLOR="%{\033[5;31m%}"
    fi

    if [[ ! $REF = '' ]]; then
        OUTPUT="$OUTPUT$PR_BLUE$PR_SHIFT_IN$PR_TBCORNER$PR_SHIFT_OUT$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
($PR_RED$VCS_ICON:$BRANCH_COLOR$REF$PR_RESET$PR_BLUE)"
    fi

    if [[ ! $VIRTUAL_ENV = '' ]]; then
        local VE=$(basename $(dirname $VIRTUAL_ENV))
        local VE_TEXT='ve'
        if [[ $OUTPUT = '' ]]; then
            OUTPUT="$OUTPUT$PR_SHIFT_IN$PR_TBCORNER$PR_SHIFT_OUT"
        fi
        OUTPUT="$OUTPUT$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT($PR_RED$VE_TEXT:$PR_GREEN$VE$PR_BLUE)"
    fi

    if [[ ! $OUTPUT = '' ]]; then
        echo "
$OUTPUT"
    fi
}

setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst

    ###
    # See if we can use colors.

    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	   colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
        eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
        eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
        (( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"
    PR_RESET="%{\033[0m%}"

    ###
    # See if we can use extended characters to look nicer.

    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}
    PR_TBCORNER=${altchar[n]:--}

    ###
    # Finally, the prompt.

    PROMPT='
$PR_SET_CHARSET\
$PR_BLUE$PR_SHIFT_IN$PR_ULCORNER$PR_HBAR$PR_SHIFT_OUT($PR_YELLOW%(!.%SROOT%s.%n)@%m$PR_BLUE)\
$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT($PR_MAGENTA%$PR_PWDLEN<...<%~%<<$PR_BLUE)\
$PR_SHIFT_IN${(e)PR_FILLBAR}$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\
\
$(vcsprompt)
\
$PR_BLUE$PR_SHIFT_IN$PR_LLCORNER$PR_HBAR$PR_SHIFT_OUT($PR_BLUE%D{%H:%M}$PR_LIGHT_BLUE:%(!.$PR_RED.$PR_WHITE)%#$PR_BLUE)\
$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '

    RPROMPT='$(battery_charge) $PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_HBAR$PR_SHIFT_OUT\
($PR_YELLOW%D{%a,%b%d}$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

    PS2='$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

battery_charge() {
   local BATTERY_COLOR=$PR_GREEN
   local BATTERY_PERCENT=$((`ioreg -rc "AppleSmartBattery" | awk '/CurrentCapacity/{print $3}'` * 100 / `ioreg -rc "AppleSmartBattery" | awk '/MaxCapacity/{print $3}'`))
   if [[ $BATTERY_PERCENT -lt 66 ]]; then
       BATTERY_COLOR=$PR_YELLOW
   fi
   if [[ $BATTERY_PERCENT -lt 33 ]]; then
       BATTERY_COLOR=$PR_RED
   fi

   echo -n "$BATTERY_COLOR"
   for i in 10 20 30 40 50 60 70 80 90 100; do
       if [[ $i -gt $BATTERY_PERCENT ]]; then
           echo -n "▹"
       else
           echo -n "▸"
       fi
   done
}

# Run these the first time this script is parsed.
chpwd
setprompt
settitle