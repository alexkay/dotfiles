
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls="ls --color=auto"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
    
# HACKS

export BANSHEE_DEV_OPTIONS="--db=$HOME/.config/banshee-1/banshee.dev.db --gconf-base-key=/apps/banshee-1-dev/"
export GIT_PS1_SHOWDIRTYSTATE='true' # doesn't seem to work
export PS1='\[\033[1;32m\]\w\[\033[0m\]$(__git_ps1 "(%s)")$ '
export EDITOR="emacsclient"
export VISUAL="emacsclient"
alias e="emacsclient"
alias ec="emacsclient -c"
alias ecn="emacsclient -c -n"
alias en="emacsclient -n"
alias et="emacsclient -t"
