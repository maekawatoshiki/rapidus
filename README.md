# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine (aiming to pass test262)

# Features 

- Small
- Partly support for Tracing-JIT compiling

# Building from Source

## Building on Linux

1. Install Rust

  Run the command below and follow the onscreen instructions. 

```sh
curl https://sh.rustup.rs -sSf | sh
```

2. Use Rust Nightly

```sh
rustup override set nightly
```

3. Install dependencies
  - LLVM 6.0
  - (Other packages as necessary...)
  
```sh
# e.g. Ubuntu or Debian
apt-get install llvm-6.0
```

4. Test 

```sh
cargo test
```

- If the compilation failed because of LLVM-related errors, the following command may help.

```sh
ln -sf /usr/bin/llvm-config-6.0 /usr/bin/llvm-config
```

5. Build

```sh
cargo run --release
```

6. Run

```sh
cargo run --release example/XXX.js
```

## Building on other platforms

I don't know.

- ~~tips: If you are using macOS, you cannot use llvm installed with ``brew``. You should use macports or docker instead.~~ Now it works!

## Use Dockerfile

- Docker image: uint256/rapidus

```sh
$ docker build -t rapidus:0.1.1 .
$ docker run -it rapidus:0.1.1
```
