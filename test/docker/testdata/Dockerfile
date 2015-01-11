from debian:unstable

run apt-get -q -y update && \
    apt-get install -y \
    curl \
    git-core

run mkdir /testdata
workdir /testdata

run git clone --depth 1  --branch v0.11.0 \
    https://github.com/bbatsov/projectile.git someuser/repos/github/projectile

run git clone --progress --depth 1 --branch v3.17.2 \
    git://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git someuser/repos/linux/

run mkdir -p someuser/downloads/source/emacs/ && \
    cd someuser/downloads/source/emacs/ && \
    curl -L http://ftp.df.lth.se/pub/ftp.gnu.org/pub/gnu/emacs/emacs-24.4.tar.gz | tar xz

run mkdir -p nofiles/shallow/c/d/e/f/g/h/medium/j/k/l/m/n/o/p/q/r/s/t/u/v/x/deep/edge/

workdir /
