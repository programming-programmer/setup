#!/bin/bash

# Adds `~/.local/bin` to $PATH
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Adds /usr/local/go to $PATH
export PATH=$PATH:/usr/local/go/bin

# JAVA
JAVA_HOME='/usr/java/jdk-20.0.1/'
export PATH=$PATH:/usr/java/jdk-20.0.1/bin

# Default programs:
export EDITOR="$HOME/.local/share/applications/nvim-linux64/bin/nvim"

# Clean-up, clean up, everybody everywhere!
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
. "$HOME/.cargo/env"
