
rm -rf git-pull
mkdir git-pull

# clone..
cat pull.list | (cd git-pull; while read x; do git clone https://github.com/janestreet/$x; done)

# and fixup
(cd git-pull; tar -xf ../missing-jbuilds-etc.tar)
find . -name discover.sh | xargs chmod a+x
cat 4.02-fixes.diff | (cd git-pull; patch -p1)
