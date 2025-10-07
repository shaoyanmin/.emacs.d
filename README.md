# Install Emacs from source code

```bash
# Debian 12
sudo apt-get install -y libgccjit-12-dev libtree-sitter-dev libmagickwand-dev libmagickcore-dev

wget https://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-30.2.tar.xz
tar xvf ./emacs-30.2.tar.xz
cd emacs-30.2/

./autogen.sh
./configure --with-mailutils  --with-tree-sitter  --with-imagemagick --with-cairo CFLAGS="-O3 -march=native"
nproc
make -j4
sudo make install
```
