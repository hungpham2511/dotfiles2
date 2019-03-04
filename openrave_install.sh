mkdir -p ~/git; cd ~/git
git clone https://github.com/crigroup/openrave-installation.git
cd openrave-installation
./install-dependencies.sh
./install-fcl.sh
./install-openrave.sh
sudo apt install python-tk
pip install --ignore-installed cython matplotlib coverage python-coveralls cvxopt tabulate --user

