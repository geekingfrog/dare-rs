# To run this test:
# Generate the file atomic_struct.py from the schema atomic_struct.dare
# `cargo run -- --schema tests/atomic_struct.dare --target-dir tests/python
# run `pytest .`

import json
import pytest
from operator import itemgetter
from . import atomic_struct, simple_enum


def gather_tests(spec_name):
    with open(spec_name) as fd:
        spec = json.load(fd)
    return spec["tests"]


@pytest.mark.parametrize(
    "test_spec",
    gather_tests("../atomic_struct_spec.json")["Customer"],
    ids=itemgetter("description"),
)
def test_atomic_struct_customer(test_spec):
    if test_spec["valid"]:
        atomic_struct.Customer.from_json(test_spec["data"])
    else:
        with pytest.raises(atomic_struct.ValidationError):
            atomic_struct.Customer.from_json(test_spec["data"])


@pytest.mark.parametrize(
    "test_spec",
    gather_tests("../atomic_struct_spec.json")["Student"],
    ids=itemgetter("description"),
)
def test_atomic_struct_student(test_spec):
    if test_spec["valid"]:
        atomic_struct.Student.from_json(test_spec["data"])
    else:
        with pytest.raises(atomic_struct.ValidationError):
            atomic_struct.Student.from_json(test_spec["data"])


@pytest.mark.parametrize(
    "test_spec",
    gather_tests("../simple_enum_spec.json")["SimpleEnum"],
    ids=itemgetter("description"),
)
def test_simple_enum(test_spec):
    if test_spec["valid"]:
        simple_enum.SimpleEnum.from_json(test_spec["data"])
    else:
        with pytest.raises(simple_enum.ValidationError):
            simple_enum.SimpleEnum.from_json(test_spec["data"])
