from dare.json_directives import EnumUnion, UnionSimple1, UnionString


def test_first_variant() -> None:
    got = EnumUnion.from_json(None)
    assert (
        got == UnionSimple1()
    ), "First variant is constructed, even though second variant matches as well"

def test_first_variant_data() -> None:
    got = EnumUnion.from_json("not b64")
    assert got == UnionString("not b64")
