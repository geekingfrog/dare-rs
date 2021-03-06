################################################################################
# Everything below is the python prelude, automatically inserted
# by the dare compiler.
################################################################################


class ValidationError(Exception):
    def __init__(
        self, message: Union[str, List[Any], Dict[str, Any]], data: Optional[Any] = None
    ):
        self.messages = [message] if isinstance(message, (str, bytes)) else message
        self.data = data
        super().__init__(message)


# typevars are produced by the caller
# T = TypeVar("T")
# K = TypeVar("K")
# V = TypeVar("V")


def parse_primitive(prim_type: Union[type, Tuple[type, ...]], msg: str, x: Any) -> V:
    """Parse a non nullable primitive type"""
    if x is None:
        raise ValidationError(message="Missing data for required field", data=x)
    if not isinstance(x, prim_type):
        raise ValidationError(message=msg, data=x)
    return cast(V, x)


def parse_str(x: Any) -> str:
    return parse_primitive(str, "Not a valid string", x)


def parse_bool(x: Any) -> bool:
    return parse_primitive(bool, "Not a valid boolean", x)


def parse_int(x: Any) -> int:
    if x is True or x is False:
        raise ValidationError(message="{} is not an integer".format(x))
    return parse_primitive(int, "Not a valid integer", x)


def parse_float(x: Any) -> float:
    return parse_primitive((float, int), "Not a valid float", x)


def parse_bytes(x: Any) -> bytes:
    raw = parse_str(x)
    try:
        return base64.b64decode(raw)
    except binascii.Error as e:
        raise ValidationError(message="Invalid base64 encoding: {}".format(e), data=x)


def parse_optional(parse: Callable[[Any], V], val: Any) -> Optional[V]:
    if val is None:
        return None
    return parse(val)


def parse_list(parse: Callable[[Any], V], l: Any) -> List[V]:
    errors = {}
    result = []
    if not isinstance(l, list):
        raise ValidationError(message="Not a list")
    for idx, x in enumerate(l):
        try:
            result.append(parse(x))
        except ValidationError as e:
            errors[str(idx)] = e.messages
    if errors:
        raise ValidationError(message=errors, data=l)
    return result


def parse_singleton(parse: Callable[[Any], V], l: Any) -> V:
    """
    parse a list of length 1 (useful for sum types)
    """
    parsed = parse_list(parse, l)
    if len(parsed) != 1:
        raise ValidationError(
            message="Must have exactly one element but got {}".format(len(parsed)),
            data=l,
        )
    return parsed[0]


def parse_object(parse_val: Callable[[Any], V], x: Any) -> Dict[str, V]:
    errors: Dict[str, Any] = {}
    result = {}

    if not isinstance(x, dict):
        raise ValidationError(
            message="Expected a dictionnary but got {}".format(type(x)), data=x
        )

    for k, v in x.items():
        try:
            key = parse_str(k)
        except ValidationError as e:
            errors[k] = {"key": e.messages}

        try:
            val = parse_val(v)
        except ValidationError as e:
            if k not in errors:
                errors[k] = {}
            errors[k]["value"] = e.messages

        if k not in errors:
            result[key] = val

    if errors:
        raise ValidationError(message=errors, data=x)

    return result


def parse_map(
    parse_key: Callable[[Any], K], parse_val: Callable[[Any], V], x: Any
) -> Dict[K, V]:
    """
    Parse a map out of a json dict, or a list of pairs.
    """

    errors: Dict[str, Any] = {}
    result = {}

    if not isinstance(x, list):
        raise ValidationError(
            message="Expected a dictionnary or array of tuple but got {}".format(
                type(x)
            ),
            data=x,
        )

    for idx, val in enumerate(x):
        if not isinstance(val, list):
            errors[str(idx)] = {"value": ["Not a list"]}
        elif len(val) != 2:
            errors[str(idx)] = {
                "value": ["List must have 2 elements (one key, one value)"]
            }
        else:
            stridx = str(idx)
            k, v = val
            try:
                key = parse_key(k)
            except ValidationError as e:
                errors[stridx] = {"key": e.messages}

            try:
                val = parse_val(v)
            except ValidationError as e:
                if k not in errors:
                    errors[stridx] = {}
                errors[stridx]["value"] = e.messages

            if stridx not in errors:
                result[key] = val

    if errors:
        raise ValidationError(message=errors, data=x)
    return result


def dump_bytes(v: bytes) -> str:
    return base64.b64encode(v).decode("utf-8")


def dump_optional(dump: Callable[[T], Any], v: Optional[T]) -> Any:
    if v is None:
        return v
    return dump(v)


def dump_list(dump_item: Callable[[T], Any], v: List[T]) -> List[Any]:
    return [dump_item(x) for x in v]


def dump_object(dump_item: Callable[[T], Any], d: Dict[str, T]) -> Dict[str, Any]:
    return {k: dump_item(v) for k, v in d.items()}


# d: Any, because mypy isn't smart enough and choke on some usages of dump_map
# For example, in `dump_map(identity, identity, x)`, `identity` has a different
# type on each callsite, but mypy unify the type and infer that x must be of
# type Dict[T, T] where it should be Dict[K, V]
# So instead of fighting that, just slap an Any, and assume it's going to work out fine
# (with some tests)
def dump_map(
    dump_key: Callable[[K], Any], dump_val: Callable[[V], Any], d: Any
) -> List[Any]:
    return [[dump_key(k), dump_val(v)] for k, v in d.items()]


def identity(x: T) -> T:
    return x


def get_dump_function(x: T) -> Callable[[T], Any]:
    """
    return a function to convert a given type T into json through reflection
    """
    if x is None:
        return identity

    if hasattr(x, "to_json"):
        return lambda x: x.to_json()  # type: ignore

    if isinstance(x, list):
        if not x:
            return identity
        # assume homogeneous lists, which is true for dare
        dump_fn = get_dump_function(x[0])
        return lambda x: dump_list(dump_fn, x)  # type: ignore

    if isinstance(x, tuple):
        if not x:
            return identity
        return lambda x: dump_list(lambda x1: get_dump_function(x1)(x1), x)  # type: ignore

    if isinstance(x, collections.abc.Mapping):
        if not x:
            return identity
        # assume all keys and values have the same type (true for dare)
        first_key, first_item = next(iter(x.items()))
        dump_val = get_dump_function(first_item)
        if type(first_item) == str:
            return lambda x: dump_object(lambda x2: dump_val(x2), x)  # type: ignore
        dump_key = get_dump_function(first_key)
        return lambda x: dump_map(dump_key, dump_val, x)

    if isinstance(x, bytes):
        return dump_bytes  # type: ignore

    return identity
