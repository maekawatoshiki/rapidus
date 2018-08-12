FROM jimmycuadra/rust:latest

RUN \
  # sed -i".bak" -e 's/\/\/archive.ubuntu.com/\/\/ftp.jaist.ac.jp/g' /etc/apt/sources.list && \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install zlib1g-dev -y && \
  apt-get install clang-4.0 clang-4.0-doc libclang-common-4.0-dev libclang-4.0-dev libclang1-4.0 libclang1-4.0-dbg libllvm4.0 libllvm4.0-dbg llvm-4.0 llvm-4.0-dev llvm-4.0-doc llvm-4.0-examples llvm-4.0-runtime clang-format-4.0 python-clang-4.0 opt libedit-dev build-essential make -y; \
  ln -s /usr/bin/llvm-config-4.0 /usr/bin/llvm-config;


ADD . /opt/rapidus

WORKDIR /opt/rapidus

RUN cargo test
