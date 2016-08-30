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
