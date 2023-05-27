# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Bash History
HISTCONTROL=ignoreboth

shopt -s histappend

HISTSIZE=100
HISTFILESIZE=200

# Check the Window Size 
shopt -s checkwinsize

# Aliases and bindings
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

bind '"\C-o":"lfcd\C-m"'


# enable programmable completion features 
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# the beautiful bash prompt
eval "$(starship init bash)"

# lfcd script
LFCD="/home/mthich15/.config/lf/lfcd.sh"
if [ -f "$LFCD" ]; then
    source "$LFCD"
fi

#just testing something
