# for now, the prelude is bundled with the generated code
# so grab a random file
import dare.atomic_struct as p
import pytest
import math
from typing import Any


class TestParseBool:
    def test_ok(self) -> None:
        assert p.parse_bool(True) == True
        assert p.parse_bool(False) == False

    @pytest.mark.parametrize("x", [1, None, "hello", "ZGVhZGJlZWY=", 1.2, math.nan])
    def test_not_valid(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_bool(x)


class TestParseInt:
    @pytest.mark.parametrize("i", [-1, 0, 1, 1237893821938719])
    def test_ok(self, i: Any) -> None:
        assert p.parse_int(i) == i

    @pytest.mark.parametrize("x", [True, None, "hello", "ZGVhZGJlZWY=", 1.2, math.nan])
    def test_not_int(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_int(x)


class TestParseFloat:
    @pytest.mark.parametrize("i", [-1, 0, 1, 1237893821938719, 1.2, 0, -0, math.nan])
    def test_ok(self, i: Any) -> None:
        parsed = p.parse_float(i)
        assert parsed == i or math.isnan(parsed)

    @pytest.mark.parametrize("x", [True, None, "hello", "ZGVhZGJlZWY="])
    def test_not_int(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_int(x)


class TestParseList:
    @pytest.mark.parametrize("x", [0, 1.2, math.nan, {b"foo", 2}])
    def test_not_list(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_list(p.parse_int, x)

    def test_ok_list(self) -> None:
        parsed = p.parse_list(p.parse_int, [1, 2])
        assert parsed == [1, 2]

    def test_parse_singleton_incorrect_size(self) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_singleton(p.parse_int, [1, 2])

    def test_parse_singleton_ok(self) -> None:
        parsed = p.parse_singleton(p.parse_int, [1])
        assert parsed == 1


class TestParseObject:
    @pytest.mark.parametrize("x", [0, 1.2, math.nan, {b"foo", 2}, [["foo", 1]]])
    def test_not_object(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_object(p.parse_int, x)

    @pytest.mark.parametrize("x", [1.2, math.nan, b"blah", {}, [], True])
    def test_wrong_type_value(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_object(p.parse_int, {"key": x})

    @pytest.mark.parametrize("x", [(2, p.parse_int), (True, p.parse_bool)])
    def test_valid_object(self, x: Any) -> None:
        x_val, x_parse = x
        obj = {"foo": x_val}
        assert p.parse_object(x_parse, obj) == obj

    def test_nested_object(self) -> None:
        obj = {"foo": {"bar": ["hello"]}}
        parse = lambda x: p.parse_object(lambda x2: p.parse_list(p.parse_str, x2), x)
        assert p.parse_object(parse, obj) == obj


class TestParseMap:
    @pytest.mark.parametrize("x", [0, 1.2, math.nan, {b"foo", 2}])
    def test_not_map(self, x: Any) -> None:
        with pytest.raises(p.ValidationError):
            p.parse_map(p.parse_str, p.parse_int, x)

    @pytest.mark.parametrize("x", [0, 1.2, math.nan, {b"foo", 2}])
    def test_invalid_key_type(self, x: Any) -> None:
        raw = [[x, 1]]
        with pytest.raises(p.ValidationError):
            p.parse_map(p.parse_bytes, p.parse_int, raw)

    def test_valid_map(self) -> None:
        raw = [["Zm9v", 1], ["YmFy", 2]]
        expected = {b"foo": 1, b"bar": 2}
        assert p.parse_map(p.parse_bytes, p.parse_int, raw) == expected


class TestExceptions:
    @pytest.mark.parametrize(
        "parse", [p.parse_bool, p.parse_int, p.parse_float, p.parse_str, p.parse_bytes]
    )
    def test_primitive_parse_raises_correct_attrs(self, parse: Any) -> None:
        data = {"key": "not a primitive"}
        with pytest.raises(p.ValidationError) as exc:
            parse(data)

        assert hasattr(exc.value, "messages")
        assert isinstance(exc.value.messages, list)
        assert hasattr(exc.value, "data")
        assert exc.value.data == data

    def test_parse_optional_exc(self) -> None:
        with pytest.raises(p.ValidationError) as exc:
            p.parse_optional(p.parse_bool, "not a bool")

        assert hasattr(exc.value, "messages")
        assert isinstance(exc.value.messages, list)
        assert hasattr(exc.value, "data")
        assert exc.value.data == "not a bool"

    def test_parse_list_exc(self) -> None:
        with pytest.raises(p.ValidationError) as exc:
            p.parse_list(p.parse_bool, ["nope", True, "nope again"])

        assert hasattr(exc.value, "messages")
        assert isinstance(exc.value.messages, dict)
        assert "0" in exc.value.messages
        assert "2" in exc.value.messages
