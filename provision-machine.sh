set -eu

# install stuffs
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt-get update

# basic stuffs
sudo apt-get install -y --no-install-recommends stow tmux awesome awesome-extra curl \
     emacs26 zsh

# zsh
# install core zsh shell
sudo chsh -s $(which zsh)
# oh-my-zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# remove zshrc if exist
if [ -f ~/.zshrc ]
then 
    rm ~/.zshrc 
fi


sudo apt-get install fonts-inconsolata -y
sudo fc-cache -fv

# urxvt
sudo apt-get install rxvt-unicode -y  # installation
sudo update-alternatives --set x-terminal-emulator /usr/bin/urxvt  # set default terminal emulator

# pip
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py --user

# virtual environment wrapper for python
python -m pip install virtualenvwrapper --user

# make symlinks
stow vim
stow tmux
stow zsh
stow urxvt
stow awesomewm
stow emacs

