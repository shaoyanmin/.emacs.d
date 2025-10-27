# Install Emacs from source code

```bash
# Debian 12
sudo apt-get install -y libgccjit-12-dev libtree-sitter-dev libmagickwand-dev libmagickcore-dev

wget https://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-30.2.tar.xz
tar xvf ./emacs-30.2.tar.xz
cd emacs-30.2/

./autogen.sh
./configure --with-mailutils  --with-tree-sitter  --with-imagemagick --with-cairo CFLAGS="-O3 -march=native"

# OR
./configure --with-mailutils  --with-tree-sitter  --with-imagemagick --with-cairo --with-native-compilation --with-json CFLAGS="-O3 -march=native"

nproc
make -j4
sudo make install
```

## TODO
* Try superchat + gpt.el, [[source]](https://x.com/lijigang_com/status/1974321856485470302)
  - https://github.com/yibie/superchat

## Troubleshoot
* Tree sitter version error
```elisp
```
