#!/bin/bash
cargo test
cargo kcov --verbose
bash <(curl -s https://codecov.io/bash) -s ./target/cov -t $1
