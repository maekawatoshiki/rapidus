FROM jimmycuadra/rust:latest

RUN \
  # sed -i".bak" -e 's/\/\/archive.ubuntu.com/\/\/ftp.jaist.ac.jp/g' /etc/apt/sources.list && \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install zlib1g-dev -y && \
  apt-get install clang-6.0 clang-6.0-doc libclang-common-6.0-dev libclang-6.0-dev libclang1-6.0 libclang1-6.0-dbg libllvm6.0 libllvm6.0-dbg llvm-6.0 llvm-6.0-dev llvm-6.0-doc llvm-6.0-examples llvm-6.0-runtime clang-format-6.0 python-clang-6.0 opt libedit-dev build-essential make -y; \
  ln -s /usr/bin/llvm-config-6.0 /usr/bin/llvm-config;


ADD . /opt/rapidus

WORKDIR /opt/rapidus

RUN rustup override set nightly; \
    cargo test
