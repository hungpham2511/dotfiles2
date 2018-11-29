# install stuffs
sudo add-apt-repository ppa:klaus-vormweg/awesome
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update

# basic stuffs
sudo apt install stow tmux

# awesomewm
sudo apt install -y awesome awesome-extra

# zsh
sudo apt install zsh -y
chsh -s $(which zsh)
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# emacs
sudo apt install emacs26

# urxvt
sudo apt install rxvt-unicode -y
sudo update-alternatives --set x-terminal-emulator /usr/bin/urxvt

# make symlinks
stow vim
stow tmux
stow zsh
stow urxvt
stow awesome
stow emacs

