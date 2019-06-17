# Rapidus

[![CircleCI](https://circleci.com/gh/maekawatoshiki/rapidus.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/rapidus)
[![codecov](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/rapidus/branch/master)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A toy JavaScript engine (now aiming for ES6).

# What's this branch?

In this branch:

- Develop the new generation of Rapidus (old gen in master branch)
- Some information in this README may vary from the fact

Issues are always welcome



# Rapidus on WASM

On [this page](https://maekawatoshiki.github.io/rapidus), you can try rapidus compiled into WASM on your browser. How amazing, isn't it?

*The compiled rapidus on the above page is some commits behind this branch.*

# Features 

- Small
- Partly support for Tracing-JIT compiling
- REPL 

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
cargo run --release examples/XXX.js
```

7. multilined-aware REPL

```sh
$ cargo run
> function fact(n) {
... if (n < 2) {     <- recognize multilined input
... return n
... } else {
... return n * fact(n-1)
... }
... }                <- recognize the end of input
undefined
> fact(10)
3628800
```

8. Debug mode (tracing bytecode execution)
   
   use --trace option.

```sh
$ cargo run -- --trace
> function fibo(x) { if (x<2) return 1; return fibo(x-1)+fibo(x-2)}
00020m 00000 Return                    <empty>
undefined
> fibo(3)
00009m 00000 PushInt8 3                <empty>
00015m 00002 GetValue 'fibo'           3.0
00066m 00007 Call 1                    Function
--> call function
  module_id:0 func_id:0
00007m 00000 GetValue 'x'              <empty>
00001m 00005 PushInt8 2                3.0
00013m 00007 Lt                        2.0

...

00001m 00007 Lt                        2.0
00000m 00008 JmpIfFalse 00016          true
00000m 00013 PushInt8 1                <empty>
00167m 00015 Return                    1.0
<-- return value(1.0)
  module_id:0 func_id:2
00001m 00052 Add                       1.0
00044m 00053 Return                    3.0
<-- return value(3.0)
  module_id:0 func_id:0
00000m 00012 Return                    3.0
3
> 
   |     |     |                        | 
   |     |     |                        \- value at the top of exec stack
   |     |     \-------------------------- instruction
   |     \-------------------------------- program counter
   \-------------------------------------- execution time per inst. (in microsecs)
```

## Building on other platforms

I don't know.

- ~~tips: If you are using macOS, you cannot use llvm installed with ``brew``. You should use macports or docker instead.~~ Now it works!

## Use DLLs written in Rust

**THIS FEATURE IS EXPERIMENTAL**



1. Make a cargo project in the directory rapidus' directory is located

```sh
$ cargo new hello --lib
$ ls
rapidus hello
```

2. Edit ``Cargo.toml``

```sh
$ cd hello
$ <YOUR EDITOR> Cargo.toml
```

```toml
# Add the followings to Cargo.toml

[dependencies]
rapidus = { path = "../rapidus" }
# other dependencies if you want...

[lib]
name = "hello"
crate_type = ["cdylib"] # try 'dylib' if it doesn't work.
```

3. Edit ``src/lib.rs``

```sh
$ <YOUR EDITOR> src/lib.rs
```

```rust
// src/lib.rs

#[macro_use]
extern crate rapidus;
use rapidus::{
   gc,
   vm::{callobj::CallObject, error::RuntimeError, value::*, vm::VM},
};

#[no_mangle]
fn initialize(vm: &mut VM, _: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    // make_object!() is useful
    let module_exports = make_object!(
        greet:   Value::default_builtin_function(greet),
        message: Value::String("hello".to_string())
    );

    vm.set_return_value(module_exports); // We have to return module.exports

    Ok(())
}

#[no_mangle]
fn greet(vm: &mut VM, _: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    println!("Hello World from Rust DLL!");

    vm.set_return_value(Value::Undefined); // Remember to return a value you want

    Ok(())
}
```

4. Let's build

```sh
$ cargo build # --release as necessary
```

5. Copy the generated DLL to rapidus' directory

```sh
$ cp ./target/debug/libhello.(so|dll|dylib) ../rapidus
$ cd ../rapidus
$ ls
libhello.(so|dll|dylib) etc...
```

6. You're ready to use it from rapidus. Let's try from REPL.

```sh
$ cargo run
> var mod = require('hello')
> mod.greet()
Hello World from Rust DLL!
> mod.message
'hello'
```

7. Now everything can be possible from Rust!

## Use Dockerfile

- Docker image: uint256/rapidus

```sh
$ docker build -t rapidus:0.1.1 .
$ docker run -it rapidus:0.1.1
```
