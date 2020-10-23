# To run this test:
# Generate the file atomic_struct.py from the schema atomic_struct.dare
# `cargo run -- --schema tests/atomic_struct.dare --target-dir tests/python
# run `pytest .`

import json
import pytest
from operator import itemgetter
from dare import (
    atomic_struct,
    simple_enum,
    simple_sum,
    references,
    json_directives,
    nested,
    typeof,
    generic_struct,
)
from typing import Any, List, Tuple, TypedDict, cast


class Test(TypedDict):
    __test__ = False  # type: ignore

    description: str
    valid: bool
    data: Any


class Spec(TypedDict):
    description: str
    test: Test
    obj: str
    module: Any


def gather_tests(specs_and_modules: List[Tuple[str, Any]]) -> List[Spec]:
    tests = []
    for spec_name, module in specs_and_modules:
        with open(spec_name) as fd:
            spec = json.load(fd)
        for test_name, ts in spec["tests"].items():
            for t in ts:
                tests.append(
                    cast(
                        Spec,
                        {
                            "description": t["description"],
                            "test": t,
                            "obj": test_name,
                            "module": module,
                        },
                    )
                )
    return tests


@pytest.mark.parametrize(  # type: ignore
    "test_spec",
    gather_tests(
        [
            ("../atomic_struct_spec.json", atomic_struct),
            ("../simple_enum_spec.json", simple_enum),
            ("../simple_sum_spec.json", simple_sum),
            ("../references_spec.json", references),
            ("../json_directives_spec.json", json_directives),
            ("../nested_spec.json", nested),
            ("../typeof_spec.json", typeof),
            ("../generic_struct_spec.json", generic_struct),
        ]
    ),
    ids=itemgetter("description"),
)
def test_specs(test_spec: Spec) -> None:

    test = test_spec["test"]
    func = getattr(test_spec["module"], test_spec["obj"]).from_json
    if test["valid"]:
        parsed = func(test["data"])
        assert tuple_to_array(parsed.to_json()) == test["data"]
    else:
        with pytest.raises(getattr(test_spec["module"], "ValidationError")):
            func(test["data"])


def tuple_to_array(x: Any) -> Any:
    """
    doing a == b for json object will raise error when comparing
    tuple with array. The `to_json` produced by dare is using
    tuples for sum types, which will get converted to arrays
    when using json.dumps.
    Convert any tuple into array so we can test against the json spec.
    """
    if isinstance(x, (tuple, list)):
        return [tuple_to_array(v) for v in x]
    if isinstance(x, dict):
        return {tuple_to_array(k): tuple_to_array(v) for (k, v) in x.items()}
    return x
