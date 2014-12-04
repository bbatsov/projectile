from debian:jessie
run apt-get -q -y update
run apt-get install -y polipo
copy config /etc/polipo/config
expose 8123
cmd polipo
