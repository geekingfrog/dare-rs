# Dare

Dare is an [interface definition language](https://en.wikipedia.org/wiki/Interface_description_language) (IDL) with the following design goals:


## Design goals

* Target JSON (and msgPack as a stretch goal).
* Customizable representation to work with existing system.
* Type system with struct/record, sum types and polymorphism.
* Produce reasonably idiomatic code.

Its purpose is to provide simple but expressive schema definition that can be progressively integrated in an existing system without major, sweeping changes.

For more info, see the work in progress [specification](./spec.md).

## Example
Here's what dare can look like:

```
// a generic Result type
enum Result<OkType, ErrType> {
  Ok(OkType),
  Err(ErrType),
}

// A specialised Result for a particular use
type MyResult Result<String, Int>

// a record/struct/product
struct Response {
  timestamp: String, //note: datetime iso-8801
  value: MyResult
}
```

This schema can produce the following JSON data:
```json
{
  "timestamp": "2020-11-30T06:18:15Z",
  "value": {
    "tag": "Ok",
    "contents": "All is well"
  }
}
```

## Dare compiler

Currently, the only supported backend is python (and still WIP).
