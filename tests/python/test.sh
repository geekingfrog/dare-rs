#!/usr/bin/env bash

set -euo pipefail

# mkdir -p dare
# cargo run -- --schema ../atomic_struct.dare --target-dir ./dare
# cargo run -- --schema ../simple_enum.dare --target-dir ./dare
# cargo run -- --schema ../simple_sum.dare --target-dir ./dare
# cargo run -- --schema ../references.dare --target-dir ./dare
# cargo run -- --schema ../json_directives.dare --target-dir ./dare

if [ ! -f ./venv/bin/pytest ]; then
  python -m venv venv
  ./venv/bin/pip install pytest
fi

./venv/bin/pytest -vv .

if [ ! -f ./venv/bin/mypy ]; then
  python -m venv venv
  ./venv/bin/pip install mypy
fi
./venv/bin/mypy --strict .
