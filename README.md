# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine

# Features 

- Small
- Support Tracing-JIT compiling 
  - A function fitting for the following rules is JIT-compiled. 
    - Accessing only its arguments and local variables (not global variables) 
    - Only Numbers and Booleans are used
    - Calling only itself

# Run

```sh
$ cargo run example/XXX.js --easy-run
```
