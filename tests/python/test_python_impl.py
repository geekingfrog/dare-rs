from dare.json_directives import EnumUnion, UnionSimple1, UnionString
from dare import nested, typeof, simple_sum


def test_first_variant() -> None:
    got = EnumUnion.from_json(None)
    assert (
        got == UnionSimple1()
    ), "First variant is constructed, even though second variant matches as well"


def test_first_variant_data() -> None:
    got = EnumUnion.from_json("not b64")
    assert got == UnionString("not b64")


def test_parse_from_json_nested() -> None:
    """
    make sure that any `from_json` is done in the right place
    and that constructors accept already constructed objects
    """
    inner = nested.BaseStruct("hellobase")
    mb_inner = nested.BaseStruct("maybe hello")
    composite = nested.CompositeStruct(field=1, inner=inner, mb_inner=None)
    assert composite.inner.base == "hellobase"
    assert composite.mb_inner is None

    composite2 = nested.CompositeStruct(field=2, inner=inner, mb_inner=mb_inner)
    assert composite2.mb_inner is not None
    assert composite2.mb_inner.base == "maybe hello"

    assert composite2.to_json() == {
        "field": 2,
        "inner": {"base": "hellobase"},
        "mb_inner": {"base": "maybe hello"},
    }


def test_simple_sum_ctors() -> None:
    assert simple_sum.OneArg.from_json(2) == simple_sum.OneArg(2)
    assert simple_sum.TwoArgs.from_json(["hello", [True]]) == simple_sum.TwoArgs(
        ("hello", [True])
    )


def test_typeof_automatically_set() -> None:
    # resp = typeof.Response(result=typeof.Ok("all is well"), response_type="ok")
    resp = typeof.Response(result=typeof.Ok("all is well"))
    assert resp.to_json() == {"result": "all is well", "response_type": "ok"}
