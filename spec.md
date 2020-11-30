Work in progress of the dare specification.

# Design goals

* Type system with struct/record, sum types and polymorphism.
* Customizable representation to work with existing system.
* Target JSON (and msgPack as a stretch goal).
* Produce reasonably idiomatic code.


# Lexical conventions

- comments are `//` until end of line.
- trailing commas are authorized.
- the format is whitespace insensitive.

# Primitive types

* String
* UInt, Int: variable length unsigned and signed integer (up to 64 bits)
* Int8, Int16, Int32, Int64: signed integers
* UInt8, UInt16, UInt32, UInt64: unsigned integers
* Float: 64bit float
* Bool
* Bytes: binary blob (represented as base64 in json)

# simple record:
```
struct SimplePerson {
  name: String,
  age: Int,
}
```

gives the following json:
```json
{
  "name": "charlie",
  "age": 42
}
```

# polymorphic record:
```
struct Inventory<Item> {
  capacity: Int,
  items: [Item]
}
// that alone would not generate anything

type MyStuff Inventory<String>
```

that would give the following json:
```json
{
  "capacity": 10,
  "items": ["coucou", "moar stuff"]
}
```

# Simple enums:
```
enum Base {
  A, // Adenine
  T, // Thymine
  G, // Guanine
  C, // Cytosine
}
```

Would generate the following json:
```json
{
  "tag": "A"
}
```

# Sum types
```
enum MySum {
  A, // simple field
  B(Int),
  C(Int, List<Bool>, String), // multiple fields associated to this variant
}
```
Json:

```json
{
  "tag": "A"
}
```
```json
{
  "tag": "B",
  "contents": 42
}
```

```json
{
  "tag": "C",
  "contents": [1, [true, false], "hello"]
}
```

The tag representation can be controlled with the `as` keyword:
```
enum MySum {
  OnlyChoice(String) as "only-choice"
}
```
The corresponding json would be:
```json
{
  "tag": "only-choice",
  "contents": "this is a string"
}
```


# Polymorphism/Generics

With structs:
```
struct Inventory<Item> {
  capacity: Int,
  items: [Item]
}

type MyStuff Inventory<String>
```

that would give the following json:
```json
{
  "capacity": 10,
  "items": ["frying pan", "pen"]
}
```

With sum types:
```
enum MyResult<OkType, ErrType> {
  Ok(OkType),
  Err(ErrType)
}

type SubscriptionResult MyResult<Int, String>
```

that would give the following json:
```json
{
  "tag": "Ok",
  "contents": 42
}
```

```json
{
  "tag": "Err",
  "contents": "User already exists"
}
```

## Controlling sum type encoding with JSON directive


## As an object

The default encoding is using an object with a key for the type, and another for the content:

```
#[json(repr="object", tag="myTag", content="value")]
enum MyResult {
  Err(String),
  Ok(Int),
}
```

```json
{
  "myTag": "Err",
  "value": "Something went wrong."
}
```
```json
{
  "myTag": "Ok",
  "value": 42
}
```
The default value for the `json` directive is `json(repr="object", tag="tag", content="contents")`.


## As a tuple
Encoding sum type as tuple:
```
#[json(repr="tuple")]
enum MyResult {
  Err(String),
  Ok(Int),
  Nothing,
}
```

would produce the following json
```json
["Err", "Something went wrong."]
```
```json
["Ok", 42]
```
```json
["Nothing"]
```

## As a union
To interface with system using true union type like `String | Int`, `dare` provides:

```
#[json(repr="union")]
enum MyResult {
  Err(String),
  Ok(Int),
}
```

which would produce the following json:
```json
"Something went wrong."
```
Or:
```json
42
```

When deserializing, each variant will be attempted in order.
Simple enums (sum type where no variants have any associated data) cannot be
represented with a union type.
```
#[json(repr="union")]
enum Union {
  One,
  // the two variant is valid but will never be parsed
  // since the variant `One` will be parsed before
  Two,
  Three(String),
  // this is also valid, but also won't be parsed
  Four(String),
}
```
Variants with no data associated use `null` in json format.
Example of json for each variants:

`One` and `Two` have the same representation:
```json
null
```

`Three` and `Four` also have the same representation
```json
"three or four, who knows"
```

⚠ If you're designing new schema with dare, you should avoid these ambiguous representations.


# Embedding a sum type into a struct with `#[typeof(…)]`

```
struct MyMessage {
  version: String,
  payload: MyResult,
  messageType: #[typeof(payload)],
}
```

example of corresponding json:
```json
{
  "version": "1.0",
  "payload": {
    "tag": "Err",
    "contents": "Something went wrong."
  },
  "messageType": "Err"
}
```

This is most useful with a `union` representation for the sum type:
```
#[json(repr="union")]
enum MyResult {
  Err(String),
  Ok(Int),
}
```

```json
{
  "version": "1.0",
  "payload": "Something went wrong",
  "messageType": "Err"
}
```
In this case, the generated code will use the type in `messageType` to choose how to parse the payload.


# Nullable types
Either use the builtin type `Option<…>`
```
struct Person {
  name: String,
  // Option is builtin
  job_title: Option<String>,
}
```

Would give in json:
```json
{
  "name": "charlie",
  "job_title": null
}
```

Or the shorthand notation with `?`:
```
// this is the same as the before
struct Person {
  name: String,
  job_title: String?,
}
```

Note that the builtin `Option` type is equivalent to
```
#[json(repr="union")]
enum Option<T> {
  None,
  Some(T)
}
```

# Maps are represented as array of tuples (draft)

```
type Person Map<Int, String>;
```

in json:
```
[[5, "alice"], [3, "bob"]]
```

Special case for `Map<String, X>` where json objects are used:

```
type Person Map<String, String>;
```
would give:
```
{
  "alice": "good",
  "bob: "evil",
}
```


In case the type of the key has a string representation, one can force the use of json object

```
#[json(repr="object")]
type Person Map<Int, String>;

enum Base { A, T, G, C }

#[json(repr="object")]
type DNA Map<Base, String>;

# the following would not compile
#[json(repr="object")]
type WillFail Map<List<String>, String>;
```

in json:
```
{
  "alice": "coucou",
  "bob: "evil",
}
{
  "A": "stuff",
  "G": "more stuff"
}
```

# Examples:
Let's say a service consume some messages, with two examples below:

```json
{
  "version": "1.2",
  "sender": "account-service",
  "type": "customer-created",
  "payload": {
    "customerId": "123foobared",
    "customerName": "charlie"
  }
}
{
  "version": "1.2",
  "sender": "billing-service",
  "type": "purchaseorder-created",
  "payload": {
    "amount": 123.32,
    "poId": "po-578",
    "customerId": "123foobared"
  }
}
```
`customer-created` and the value of `payload` are linked, same for `purchaseorder-created` and the associated `payload`.

These messages can be described with the following schemas:
```
struct CustomerCreatedPayload {
  customerId: String,
  customerName: String? // anonymised customer
}

struct POCreatedPayload {
  amount: float,
  poId: String,
  customerId: String
}

#[repr(json="union")]
enum ServiceMessage {
  CustomerCreated(CustomerCreatedPayload) as "customer-created",
  PurchaseOrderCreated(POCreatedPayload) as "purchaseorder-created",
}

struct Message {
  version: String,
  sender: String,
  type: #[typeof(payload)],
  payload: ServiceMessage
}
```
