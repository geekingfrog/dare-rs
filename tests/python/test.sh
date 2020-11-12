#!/usr/bin/env bash

set -euo pipefail

mkdir -p dare
cargo run -- --schema ../atomic_struct.dare --target-dir ./dare
cargo run -- --schema ../simple_enum.dare --target-dir ./dare
cargo run -- --schema ../simple_sum.dare --target-dir ./dare
cargo run -- --schema ../references.dare --target-dir ./dare
cargo run -- --schema ../json_directives.dare --target-dir ./dare
cargo run -- --schema ../nested.dare --target-dir ./dare
cargo run -- --schema ../typeof.dare --target-dir ./dare
cargo run -- --schema ../generic_struct.dare --target-dir ./dare
cargo run -- --schema ../generic_sum.dare --target-dir ./dare

if [ ! -d ./venv ]; then
  python -m venv venv
fi

if [ ! -f ./venv/bin/pytest ]; then
  ./venv/bin/pip install "pytest==6.1.2"
fi

./venv/bin/pytest -vv .

if [ ! -f ./venv/bin/mypy ]; then
  ./venv/bin/pip install "mypy==0.790"
fi

echo "Running mypy"
./venv/bin/mypy .
