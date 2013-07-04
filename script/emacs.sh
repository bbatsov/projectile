#!/bin/sh

rm -f projectile.elc

carton exec emacs -nw -Q --directory $PWD --eval "(progn (require 'projectile) (projectile-global-mode))" $@
