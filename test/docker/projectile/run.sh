#!/bin/bash
set -e
# set -x

git init .

touch Makefile

cask exec emacs --no-site-file --no-site-lisp --batch -l test/run-bench
