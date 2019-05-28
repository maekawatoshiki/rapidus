FROM rustlang/rust:nightly

RUN set -x && \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install zlib1g-dev apt-utils -y && \
  apt-get install opt libedit-dev build-essential make -y; \
  apt-get install software-properties-common -y;

RUN set -x && \
  apt-add-repository -y "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-7 main" && \
  wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|apt-key add - && \
  apt-get update && \
  apt-get install -y llvm-7
#ln -s /usr/bin/llvm-config-7 /usr/bin/llvm-config;

RUN set -x && \
  apt-get install -y cmake g++ pkg-config jq && \
  apt-get install -y libcurl4-openssl-dev libelf-dev libdw-dev binutils-dev libiberty-dev && \
  cargo install cargo-kcov && \
  cargo kcov --print-install-kcov-sh | sh

ADD . /opt/rapidus

WORKDIR /opt/rapidus

RUN set -x && \
  export PATH=~/.cargo/bin:$PATH && \
  export PATH=~/usr/local/bin:$PATH && \
  rustup override set nightly
