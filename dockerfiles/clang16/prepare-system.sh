set -ex
tee /etc/apt/apt.conf.d/90forceyes <<EOF
APT::Get::Assume-Yes "true";
EOF
apt-get update
apt-get install cmake git clang sudo
adduser --disabled-password --gecos ci ci
echo "ci ALL=(ALL) NOPASSWD:ALL" >/etc/sudoers
