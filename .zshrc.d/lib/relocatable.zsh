BASHRC_CONFIG_DIR=$PWD
if [ $__REMOTE_TERMINAL_SCRIPT_DIR ]; then
    BASHRC_CONFIG_DIR=$__REMOTE_TERMINAL_SCRIPT_DIR
fi
export __REMOTE_TERMINAL_SCRIPT_DIR=$BASHRC_CONFIG_DIR
