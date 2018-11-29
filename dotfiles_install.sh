# install stuffs
sudo add-apt-repository ppa:klaus-vormweg/awesome
sudo apt-get update

# awesomewm
sudo apt install -y awesome awesome-extra

# zsh
sudo apt install git-core zsh -y
chsh -s $(which zsh)
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# urxvt
sudo apt install rxvt-unicode -y

# make symlinks
stow vim
stow tmux
stow zsh
stow urxvt

