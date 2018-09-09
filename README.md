# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine aiming to pass test262

# Features 

- Small
- Support Tracing-JIT compiling 
  - Currently, any functions or loops fitting for the following rules would be JIT-compiled. 
    - Accessing only its arguments and local variables (not global variables) 
    - Number and Boolean(only for function's returning type) are used
    - Calling only itself
    - (There are exceptions...)

# Building from Source

## Building on Linux

1. Install dependencies
  - LLVM 6.0
  
```sh
$ # e.g. Ubuntu or Debian
$ apt-get install llvm-6.0
```

2. Test 

```sh
$ cargo test
```

3. Build

```sh
$ cargo run --release
```

4. Run

```sh
$ cargo run --release example/XXX.js
```

## Building on other platforms

I don't know.

- ~~tips: If you are using macOS, you cannot use llvm installed with ``brew``. You should use macports or docker instead.~~ Now it works!

## Use Dockerfile

- Docker image: uint256/rapidus

```sh
$ docker build -t rapidus:1.0 .
$ docker run -it rapidus:1.0
```
