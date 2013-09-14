#!/bin/sh

ppa () {
    sudo apt-add-repository -y "$1"
}

apt_update () {
    sudo apt-get update -qq
}

apt () {
    sudo apt-get install -yy "$@"
}

# Silence debconf
export DEBIAN_FRONTEND='noninteractive'

# Bring in the necessary PPAs
ppa ppa:cassou/emacs
apt_update

# Install Emacs 24.x and Emacs snapshot
apt emacs24 emacs24-el emacs24-common-non-dfsg \
    emacs-snapshot emacs-snapshot-el

# Install Cask for Emacs dependency management
CASK_DIR=/opt/cask.el-0.4.6
if ! [ -d "$CASK_DIR" -a -x "/$CASK_DIR/bin/cask" ]; then
  sudo rm -rf "$CASK_DIR"
  wget -O - https://github.com/cask/cask/archive/v0.4.6.tar.gz | sudo tar xz -C /opt
  # Bring Cask into $PATH
  sudo ln -fs "$CASK_DIR/bin/cask" /usr/local/bin
fi
