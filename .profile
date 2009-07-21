stty erase ^H
stty ek

if [ -f /etc/profile ]; then
    # Global bashrc
	. /etc/profile
fi
