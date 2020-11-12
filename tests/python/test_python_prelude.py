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
