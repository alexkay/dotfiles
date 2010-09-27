# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/alex/.zshrc'
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# HACKS

# Git prompt: http://github.com/jcorbin/zsh-git
setopt promptsubst
autoload -U promptinit
promptinit
prompt wunjo


export BANSHEE_DEV_OPTIONS="--db=$HOME/.config/banshee-1/banshee.dev.db --gconf-base-key=/apps/banshee-1-dev/ --debug-metrics"
#export GIT_PS1_SHOWDIRTYSTATE='true' # doesn't seem to work
#export PS1='\[\033[1;32m\]\w\[\033[0m\]$(__git_ps1 "(%s)")$ '
export EDITOR="emacsclient"
export VISUAL="emacsclient"
alias e="emacsclient"
alias ec="emacsclient -c"
alias ecn="emacsclient -c -n"
alias en="emacsclient -n"
alias et="emacsclient -t"
