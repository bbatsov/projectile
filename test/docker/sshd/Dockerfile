from projectile-testdata
run apt-get -q -y update
run apt-get install -y \
    curl \
    git-core \
    openssh-server \
    trickle \
    && true

run apt-get install -y inotify-tools

run mkdir /var/run/sshd && \
    chmod 0755 /var/run/sshd
run sed -i -e s/\#PasswordAuthentication.*/PasswordAuthentication\ no/ /etc/ssh/sshd_config
copy id_rsa.pub /root/.ssh/authorized_keys
run mkdir -p /root/.ssh && \
    touch /root/.ssh/authorized_keys && \
    chmod -R 600 /root/.ssh
copy start /start
run chmod +x /start
cmd /bin/bash -c /start
expose 22
