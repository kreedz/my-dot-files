# zmodload -a zsh/zprof zprof

# exports
export EDITOR="nvim"
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export JDK_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export JRE_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre
export GEM_HOME=$HOME/.gem
export GEM_PATH=$HOME/.gem/ruby/2.1.0
PATH=$HOME/bin:/usr/local/bin:$PATH
PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin
PATH=$PATH:$JDK_HOME/bin
PATH=$PATH:$JRE_HOME/bin
PATH=$HOME/neovim/bin:$PATH
PATH=$PATH:~/.local/bin
PATH=$PATH:~/.bin
export PATH
# export WORKON_HOME=~/.virtualenvs
# source /usr/local/bin/virtualenvwrapper.sh
# source ~/.bin/tmuxinator.bash
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

alias -s {avi,mpeg,mpg,mov,m2v,mkv}=mpv

alias ..='cd ..'
alias ...='cd ../..'

# git
alias gst='git status'
alias gc='git commit -v'
alias gcmsg='git commit -m'
alias ga='git add'


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
function pip() {
  if [ "$1" = "install" -o "$1" = "bundle" ]; then
    cmd="$1"
    shift
    /usr/local/bin/pip $cmd --user $@
  else
    /usr/local/bin/pip $@
  fi
}

function sshagent() {
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


setopt autocd
unsetopt beep


# zplug
# source ~/.zplug/init.zsh
# zplug "plugins/git", from:oh-my-zsh
# zplug "plugins/sudo", from:oh-my-zsh
# zplug "plugins/pip", from:oh-my-zsh
# zplug "zsh-users/zsh-autosuggestions", at:develop
# zplug "zsh-users/zsh-syntax-highlighting", defer:3
# zplug "Tarrasch/zsh-bd"
# zplug "themes/sorin", from:oh-my-zsh, as:theme
# if ! zplug check --verbose; then
#     printf "Install? [y/N]: "
#     if read -q; then
#         echo; zplug install
#     fi
# fi

# # Then, source plugins and add commands to $PATH
# zplug load --verbose


# zgen
# source ~/.zgen/zgen.zsh
# # if the init scipt doesn't exist
# if ! zgen saved; then
#     echo "Creating a zgen save"

#     zgen oh-my-zsh

#     # plugins
#     zgen oh-my-zsh plugins/git
#     zgen oh-my-zsh plugins/sudo
#     # zgen load zsh-users/zsh-syntax-highlighting

#     # theme
#     zgen oh-my-zsh themes/arrow

#     # save all to init script
#     zgen save
# fi

# antibody
source <(antibody init)
antibody bundle zsh-users/zsh-syntax-highlighting
antibody bundle zsh-users/zsh-autosuggestions branch:develop
antibody bundle subnixr/minimal
# fpath=( "$HOME/.zfunctions" $fpath )
# antibody bundle sindresorhus/pure

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
