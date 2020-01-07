# Json

A lightweight full-featured json crate for rust.

## Features

- Encode (both primitive or custom) type to json string.
- Decode to custom type from json string.
- Index json object/array value by `&str`/`usize`.
- Use string path-syntax to access any value in your json value.
- Supports duplicate key in object value.
- Use `object!{}`/`array![]` macros to make json easily.
- Use `#[derive(JsonEncode, JsonDecode)]` for custom structs to auto implement `ToJson`/`FromJson` trait.

## Usage

### encode/decode

It's quite simple to encode/decode json in one step:

```rust
// extern crate json;

use json;

fn main() {
    let vec = vec![1, 2, 3];

    // encode Vec
    let js = json::encode(&vec);
    assert_eq!("[1, 2, 3]", &js);    

    // decode to Vec
    let vec_from_json: Vec<i32> = json::decode(&js).unwrap();
    assert_eq!(vec, vec_from_json);
  
    use std::collections::HashMap;
    let mut map = HashMap::new();
    map.insert("name".to_string(), "Jack".to_string());
    map.insert("age".to_string(), "17".to_string());

    // encode HashMap
    let js = json::encode(&map);
    assert_eq!(r#"{"age": "17", "name": "Jack"}"#, &js);

    // decode to HashMap    
    let map_from_json: HashMap<String, String> = json::decode(&js).unwrap();
    assert_eq!(map, map_from_json);
}
```

**Note: Only types that implement `ToJson`/`FromJson` trait can be encode/decode to/from json.** 
Basic types shown below has already implement `Tojson`/`Fromjson`:

ToJson:

- String/&str
- All int/float types and its reference type
- bool and &bool
- Vec<T>/&\[T\] where T: ToJson
- Vec<(K, V)>/&\[(K, V)\] where K: ToString, V: ToJson
- HashMap<K, V> where K: ToString, V: ToJson
- Option<T> where T: ToJson

FromJson:

- String
- All int/float types
- bool
- Vec<T> where T: FromJson
- Vec<(K, V)> where K: FromStr, V: FromJson
- HashMap<K, V> where K: FromStr, V: FromJson
- Option<T> where T: FromJson

### use `#[derive(JsonEncode, JsonDecode)]`

You can implement `ToJson`/`FromJson` for your own type mannually, or to use the `JsonEncode`/`JsonDecode` proc-macro.

```rust
// extern crate json;

use json::{JsonEncode, JsonDecode};

#[derive(JsonEncode, JsonDecode, PartialEq, Debug)]
pub struct Item {
    #[key = "id"]
    pub _id: i32,
    pub name: String,
    pub vec: Vec<i32>,
    #[ignore]
    opt: Option<()>,
}

fn main() {
    let item = Item {
        _id: 1,
        name: "Package".to_string(),
        vec: vec![1, 2, 3],
        opt: None,
    };

    let js = json::encode(&item);
    assert_eq!(r#"{"id": 1, "name": "Package", "vec": [1, 2, 3]}"#, &js);

    let item_from_json: Item = json::decode(&js).unwrap();
    assert_eq!(item, item_from_json);
}
```

**Note: `JsonEncode`/`JsonDecode` currently only support struct-struct, and each field should implement `ToJson`/`FromJson` trait.**

There are two attributes for `JsonEncode`/`JsonDecode`:

1. `key = <string>`: change the key name in json, default is the field name.
2. `ignore`: ignore this field. **The filed should implement `default::Default` when decoding from json.**

### get/set/del

Use string path-syntax to access value anywhere in json:

```rust
// extern crate json

use json;

fn main() {
    let js = r#"{"numbers": [1, 2, 3]}"#;
    
    // get
    let n: i32 = json::get(&js, "numbers.#0").unwrap();
    assert_eq!(n, 1);
    
    // set
    let js_after_set = json::set(&js, "numbers.#4", 5).unwrap();
    assert_eq!(&js_after_set, r#"{"numbers": [1, 2, 3, null, 5]}"#);
    
    // del
    let js_after_del = json::del(&js_after_set, "numbers.#3").unwrap();
    assert_eq!(&js_after_del, r#"{"numbers": [1, 2, 3, 5]}"#);
}
```

The syntax are:

- use `<string>` as key to access value in object.
- use `#<integer>` as index to access value in array.
- use `.` to concat nested paths. 
- use `\.` if object key contains `.`.
- use `\#` if object key starts with `#`.
- use `\\` if object key starts with `\`.

Note:

1. `get`/`del` method returns Err when the path-syntax is invalid or find nothing in json by path.
2. `set` method returns Err only when the path-syntax is invalid, the set operation will always success,
which means that if there is no object/array value, the `set` function would create a object/array value,
or if the value kind is not object/array, the `set` function would change the kind to object/array.

### `object!{}`/`array![]` macro

Use `object!{}` to create json object, use `k => v` where K: ToString, V: ToJson:

```rust
let js = object! {
    "name" => "Jack",
    "age" => 24,
};
```

Use `array![]` to create json array:

```rust
let js = array![1, 2, 3];
```

You can use both of them to create a json value:

```rust
let js = object! {
    "array" => array![1, 2, 3],
};
```

## The `Json` Type

`Json` is the main type in json crate, which looks like:

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Json {
    Object(HashMap<String, (Json, LinkedList<Json>)>),
    Array(Vec<Json>),
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}
```

**It is necessary to use a `HashMap<String, (Json, LinkedList<Json>)>` as an object value because duplicate key is allowed.
We put the last value of the same key in the zero position of `(Json, LinkedList<Json>)`, and previous values in the LinkedList.
The implementation is, push the latest value to the front of LinkedList first, and then call mem::swap(&mut v.0, &mut v.1\[0\]).**

You can parse a `&str` to Json by:

```rust
let js: Json = r#"{"list": [1, 2, 3]}"#.parse().unwrap();
```

And convert a `Json` to `String`:

```rust
let js: Json = r#"{"list": [1, 2, 3]}"#.parse().unwrap();
let s = js.to_string();
assert_eq!(r#"{"list": [1, 2, 3]}"#, &s);
```

`Json` implements `Index`/`IndexMut` trait(it panics if no value found):

```rust
let js: Json = r#"{"list": [1, 2, 3]}"#.parse().unwrap();
assert_eq!(Json::Number(1f64), js["list"][0]);
```

You can add/set/del value json by:

object:

- `Json::insert`: insert value to json object, if key exists, update that key.
- `Json::append`: append value to json object, may cause duplicate key.
- `Json::delete`: delete value by key in json object.

array:

- `Json::push`: push element to json array.
- `Json::replace`: replace element in json array.
- `Json::remove`: remove element in json array.

If you want all values of a single key, then you can use `Json::index_and_merge`:

```rust
let js: Json = r#"{"name": "Jack", "name": "Jerry"}"#.parse().unwrap();
// vec should be [&Json::String("Jack"), &Json::String("Jerry")]
let vec = js.index_and_merge("name").unwrap();
```

