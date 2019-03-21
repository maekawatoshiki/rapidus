FROM jimmycuadra/rust:latest

RUN \
  # sed -i".bak" -e 's/\/\/archive.ubuntu.com/\/\/ftp.jaist.ac.jp/g' /etc/apt/sources.list && \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install zlib1g-dev -y && \
  apt-get install llvm-6.0 opt libedit-dev build-essential make -y; \
  ln -s /usr/bin/llvm-config-6.0 /usr/bin/llvm-config;


ADD . /opt/rapidus

WORKDIR /opt/rapidus

RUN rustup override set nightly; \
    cargo test
