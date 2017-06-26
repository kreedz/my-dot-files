# zmodload -a zsh/zprof zprof

# exports
export EDITOR="nvim"
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export JDK_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export JRE_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre
export GEM_HOME=$HOME/.gem
export GEM_PATH=$HOME/.gem/ruby/2.1.0
export PYENV_ROOT="$HOME/.pyenv"
# export TERM=screen-256color
export LUA_PATH='/home/kreedz/.luarocks/share/lua/5.1/?.lua;/home/kreedz/.luarocks/share/lua/5.1/?/init.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua;/usr/share/lua/5.1/?.lua;./?.lua;/usr/local/lib/lua/5.1/?.lua;/usr/local/lib/lua/5.1/?/init.lua;/usr/share/lua/5.1/?/init.lua'
export LUA_CPATH='/home/kreedz/.luarocks/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/?.so;./?.so;/usr/lib/x86_64-linux-gnu/lua/5.1/?.so;/usr/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so'
PATH=$HOME/bin:/usr/local/bin:$PATH
PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin
PATH=$PATH:$JDK_HOME/bin
PATH=$PATH:$JRE_HOME/bin
PATH=$HOME/neovim/bin:$PATH
PATH=$PATH:~/.local/bin
PATH=$PATH:~/.bin
PATH=$PATH:~/.gem/bin
PATH="$HOME/.rbenv/bin:$PATH"
PATH="$PYENV_ROOT/bin:$PATH"
PATH=~/.npm-global/bin:$PATH
export PATH
export down=~/Downloads
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export PROJECT_HOME=$HOME/work/django
source $HOME/.local/bin/virtualenvwrapper_lazy.sh
# export ZSH=/home/kreedz/.oh-my-zsh


# aliases
alias ls='ls --color=auto'
alias l=ls
alias ll='ls -lh'
alias df='df -h'
alias du='du -h -c'
alias la='ls -A'
alias lx='ls -lXB'              # sort by extension
alias lk='ls -lSr'              # sort by size
alias lt='ls -ltr | tail -10'   # sort by date
alias h='history | grep $1'
alias off='sudo shutdown -h now'
alias xclip='xclip -selection clip'
alias music='mpd && mpdcron && ncmpcpp'
alias n=nvim
# alias mc="export TERM=screen-256color && mc && export TERM=xterm-256color"

alias -s {avi,mpeg,mpg,mov,m2v,mkv}=mpv

alias ..='cd ..'
alias ...='cd ../..'

# git
alias gst='git status'
alias gc='git commit -v'
alias gcmsg='git commit -m'
alias ga='git add'
alias gdi='git diff'

# history
HISTFILE=~/.zsh/zsh_history

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=3000

setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS


# sources
# source $ZSH/oh-my-zsh.sh
# source antigen.zsh
# source "/home/kreedz/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh"
# . ~/.zsh/exports.zsh
# . ~/.zsh/aliases.zsh
# . ~/.zsh/functions.zsh
# . ~/.zsh/history.zsh
# . ~/.zsh/sources.zsh


# functions
# pip --user install by default
# function pip() {
#   if [ "$1" = "install" -o "$1" = "bundle" ]; then
#     cmd="$1"
#     shift
#     /usr/local/bin/pip $cmd --user $@
#   else
#     /usr/local/bin/pip $@
#   fi
# }

function initsshagent() {
    SSH_ENV="$HOME/.ssh/environment"

    function start_agent {
        echo "Initialising new SSH agent..."
        /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
        echo succeeded
        chmod 600 "${SSH_ENV}"
        . "${SSH_ENV}" > /dev/null
        /usr/bin/ssh-add;
    }

    # Source SSH settings, if applicable

    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        #ps ${SSH_AGENT_PID} doesn't work under cywgin
        ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
            start_agent;
        }
    else
        start_agent;
    fi
}

function initrbenv() {
    eval "$(rbenv init -)"
}

function initpyenv() {
    eval "$(pyenv init -)"
}

function initvirtualenvwrapper() {
    source /usr/local/bin/virtualenvwrapper.sh
}





# setopt autocd
# unsetopt beep


# zplug
# source ~/.zplug/init.zsh
# zplug "plugins/git", from:oh-my-zsh
# # zplug "plugins/sudo", from:oh-my-zsh
# # zplug "plugins/pip", from:oh-my-zsh
# # zplug "zsh-users/zsh-autosuggestions", at:develop
# # zplug "zsh-users/zsh-syntax-highlighting", defer:3
# # zplug "Tarrasch/zsh-bd"
# zplug "themes/sorin", from:oh-my-zsh, as:theme
# if ! zplug check --verbose; then
#     printf "Install? [y/N]: "
#     if read -q; then
#         echo; zplug install
#     fi
# fi

# # Then, source plugins and add commands to $PATH
# zplug load --verbose

# autoload -U up-line-or-beginning-search
# autoload -U down-line-or-beginning-search
# zle -N up-line-or-beginning-search
# zle -N down-line-or-beginning-search
# bindkey "^[[A" up-line-or-beginning-search # Up
# bindkey "^[[B" down-line-or-beginning-search # Down
# bindkey '^[[A' up-line-or-search
# bindkey '^[[B' down-line-or-search
# bindkey '^[[A' up-line-or-history
# bindkey '^[[B' down-line-or-history
# bindkey '^[[1;5C' forward-word
# bindkey '^[[1;5D' backward-word
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down

# bindkey '^[[A' forward-word
# bindkey '^[[B' backward-word

# DEBIAN_PREVENT_KEYBOARD_CHANGES=yes
# zgen
# ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc)
# GEOMETRY_PROMPT_PREFIX=""
GEOMETRY_PROMPT_PLUGINS=(virtualenv git)
source ~/.zgen/zgen.zsh
# if the init scipt doesn't exist
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/pip
    # zgen oh-my-zsh plugins/django
    # zgen load soimort/translate-shell translate-shell.plugin.zsh develop
    # zgen load zsh-users/zsh-autosuggestions zsh-autosuggestions.zsh develop

    # theme
    # zgen oh-my-zsh themes/arrow
    zgen load frmendes/geometry

    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-history-substring-search

    zgen save
fi

# bindkey -e
bindkey '^[OA' history-substring-search-up
bindkey '^[OB' history-substring-search-down

# antibody
# source <(antibody init)
# antibody bundle zsh-users/zsh-syntax-highlighting
# antibody bundle zsh-users/zsh-autosuggestions branch:develop
# antibody bundle subnixr/minimal
# antibody bundle sindresorhus/pure
# antibody bundle frmendes/geometry

# filthy promt theme
# fpath=( "$HOME/.zfunctions" $fpath )
# antibody bundle molovo/filthy
# autoload -U promptinit && promptinit
# prompt filthy

# zprof


# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="sunaku"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(git sudo pip)

# antigen
# antigen use oh-my-zsh
# antigen bundle zsh-users/zsh-syntax-highlighting
# antigen bundle zsh-users/zsh-autosuggestions
# antigen theme sorin
# antigen apply

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source ~/.bin/tmuxinator.zsh
export FZF_TMUX=1
auto-ls () {
   if [[ $#BUFFER -eq 0 ]]; then
       echo ""
       ls
       echo "" # Workaround
       zle redisplay
   else
       zle .$WIDGET
   fi
}
zle -N auto-ls
zle -N accept-line auto-ls
zle -N other-widget auto-ls
# function do_enter() {
#     if [ -n "$BUFFER" ]; then
#         zle accept-line
#         return 0
#     fi
#     echo
#     ls
#     if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
#         echo
#         echo -e "\e[0;33m--- git status ---\e[0m"
#         git status -sb
#     fi
#     zle reset-prompt
#     return 0
# }
# zle -N do_enter
# bindkey '^m' do_enter
