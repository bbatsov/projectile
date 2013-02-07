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

# Install Emacs 24.2 and Emacs snapshot
apt emacs24 emacs24-el emacs24-common-non-dfsg \
    emacs-snapshot emacs-snapshot-el

# Install carton for Emacs dependency management
CARTON_DIR=/opt/carton-0.1.0
if ! [ -d "$CARTON_DIR" -a -x "/$CARTON_DIR/bin/carton" ]; then
  sudo rm -rf "$CARTON_DIR"
  wget -O - https://github.com/rejeep/carton/archive/v0.1.0.tar.gz | \
    sudo tar xz -C /opt
  # Bring carton into $PATH
  sudo ln -fs "$CARTON_DIR/bin/carton" /usr/local/bin
fi
