from projectile-testrunner

copy Cask /projectile/
copy projectile.el /projectile/
workdir /projectile
run cask
copy test/docker/projectile/run.sh /run.sh
copy . /projectile/
