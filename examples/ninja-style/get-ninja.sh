set -e
git clone git://github.com/martine/ninja.git
cd ninja
python configure.py
hg init; hg ci -qAm init
