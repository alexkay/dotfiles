
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'

# HACKS

export BANSHEE_DEV_OPTIONS="--db=$HOME/.config/banshee-1/banshee.dev.db --gconf-base-key=/apps/banshee-1-dev/"
export GIT_PS1_SHOWDIRTYSTATE='true' # doesn't seem to work
export PS1='\[\033[1;32m\]\w\[\033[0m\]$(__git_ps1 "(%s)")$ '
export EDITOR=vim
