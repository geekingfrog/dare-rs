#!/usr/bin/env bash

set -euo pipefail

cargo run -- --schema ../atomic_struct.dare --target-dir ./
cargo run -- --schema ../simple_enum.dare --target-dir ./
cargo run -- --schema ../simple_sum.dare --target-dir ./

if [ ! -f ./venv/bin/pytest ]; then
  python -m venv venv
  ./venv/bin/pip install pytest
fi

./venv/bin/pytest -vv .
