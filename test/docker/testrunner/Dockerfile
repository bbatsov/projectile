from projectile-testdata
run apt-get -q -y update
run apt-get install -y \
    emacs24-nox emacs24-el \
    git-core \
    make \
    python-minimal \
    ssh \
    trickle \
    time \
    && true

copy id_rsa /root/.ssh/
copy id_rsa.pub /root/.ssh/
copy ssh_config /root/.ssh/config
run chmod -R 600 /root/.ssh

run git clone https://github.com/cask/cask.git /cask
env PATH /cask/bin/:$PATH
run cask upgrade-cask
