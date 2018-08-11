# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine

# Features 

- Small
- Support Tracing-JIT compiling 
  - The functions fitting for the following rules are JIT-compiled. 
    - Accessing only their arguments and local variables (not global variables) 
    - Only Numbers and Booleans are used. 

# Run

```sh
$ cargo run example/XXX.js --easy-run
```
