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

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Aliases and bindings
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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

bind '"\C-o":"lfcd\C-m"'
