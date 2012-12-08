HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PAGER=less

setopt extendedglob
unsetopt beep
bindkey -e

zstyle :compinstall filename '/home/alex/.zshrc'
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

autoload -Uz compinit
compinit

# Git prompt: http://github.com/jcorbin/zsh-git
setopt promptsubst
autoload -U promptinit
promptinit
prompt wunjo

WORDCHARS=${WORDCHARS//[-\/_.]}

export PATH=$HOME/.cabal/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
export BANSHEE_DEV_OPTIONS="--db=$HOME/.config/banshee-1/banshee.dev.db --gconf-base-key=/apps/banshee-1-dev/ --debug-metrics --validate-db-schema"
export GNOME_FTP_USER=alexk
export EDITOR="emacsclient"
export VISUAL="emacsclient"
alias e="emacsclient"
alias ec="emacsclient -c"
alias ecn="emacsclient -c -n"
alias en="emacsclient -n"
alias et="emacsclient -t"
