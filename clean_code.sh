#!/bin/bash

set -e

cargo fix --allow-dirty
cargo clippy --fix --allow-dirty
cargo fmt