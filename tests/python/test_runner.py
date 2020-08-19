# To run this test:
# Generate the file atomic_struct.py from the schema atomic_struct.dare
# `cargo run -- --schema tests/atomic_struct.dare --target-dir tests/python
# run `pytest .`

import json
import pytest
from operator import itemgetter
from . import atomic_struct


def gather_tests(spec_name):
    with open(spec_name) as fd:
        spec = json.load(fd)
    return spec["tests"]


@pytest.mark.parametrize(
    "test_spec",
    gather_tests("../atomic_struct_spec.json"),
    ids=itemgetter("description"),
)
def test_atomic_struct(test_spec):
    if test_spec["valid"]:
        atomic_struct.Customer.from_json(test_spec["data"])
    else:
        with pytest.raises(atomic_struct.ValidationError):
            atomic_struct.Customer.from_json(test_spec["data"])
