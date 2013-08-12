#!/bin/sh

rm -f projectile.elc

cask exec emacs -nw -Q --directory $PWD --eval "(progn (require 'projectile) (projectile-global-mode))" $@
