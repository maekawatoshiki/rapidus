# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine

# Features 

- Small
- Support Tracing-JIT compiling 
  - Currently, a function fitting for the following rules would be JIT-compiled. 
    - Accessing only its arguments and local variables (not global variables) 
    - Only Numbers and Booleans are used
    - Calling only itself

# Building from Source

## Building on Linux

1. Install dependencies
  - LLVM 4.0
  
```sh
$ # e.g. Ubuntu or Debian
$ apt-get install llvm-4.0
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
$ cargo run --release example/XXX.js --easy-run
```

## Building on other platforms

I don't know.

## Use Dockerfile

- Docker image: uint256/rapidus

```sh
$ docker build -t rapidus:1.0 .
$ docker run -it rapidus:1.0
```
