# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"
# ZSH_THEME="spaceship"
# source "$HOME/.oh-my-zsh/custom/themes/spaceship.zsh-theme"
# ZSH_THEME="avit"

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
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

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
plugins=(git python pip web-search virtualenvwrapper k autopep8 pass vagrant z)

source $ZSH/oh-my-zsh.sh

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

# Map capslock key to control
function mapctrl {
    setxkbmap -option ctrl:nocaps
}
mapctrl

# Research code project
export PYTHONPATH="${PYTHONPATH}:${HOME}/git/robotic-CRI"
# quick compilation
alias gccp='sh /home/hung/git/robotic-CRI/cpp-learn/gccp'

# ROS
if [ -f "/opt/ros/kinetic/setup.zsh" ]
then
    source /opt/ros/kinetic/setup.zsh
else
    echo "warn: Unable to find ROS."
fi

function source_catkin_ws {
    echo "sourcing: $HOME/$1/devel/setup.zsh"
    source $HOME/$1/devel/setup.zsh
}

# source_catkin_ws catkin_ws

# Virtual ENVs
export WORKON_HOME=~/Envs
# If this does not work, try the below command instead
if [ -f "/usr/local/bin/virtualenvwrapper.sh" ]
then
    source /usr/local/bin/virtualenvwrapper.sh
else
    source $HOME/.local/bin/virtualenvwrapper.sh
fi

# Add OpenRAVE path
# THis allows us to use openravepy in virtual environment without making symlink
if [ -f "/usr/local/lib/libopenrave0.9-core.so" ] 
then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(openrave-config --python-dir)/openravepy/_openravepy_
    export PYTHONPATH=$PYTHONPATH:$(openrave-config --python-dir)
else
    echo "warn: Unable to find openrave."
fi
