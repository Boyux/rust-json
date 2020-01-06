// ...
use std::mem;
use std::ops::{Index, IndexMut};
use std::hash::Hash;
use std::str::{self, FromStr};
use std::fmt;
use std::error;
use std::collections::{HashMap, LinkedList};

// for derive macros
pub use json_derive::{JsonEncode, JsonDecode};

#[inline]
pub fn encode<T: ToJson + ?Sized>(x: &T) -> String {
    x.to_json().to_string()
}

#[inline]
pub fn decode<T: FromJson>(json: &str) -> Result<T, Error> {
    Json::parse(json)?.convert()
}

#[inline]
pub fn get<T: FromJson>(json: &str, path: &str) -> Result<T, Error> {
    Json::parse(json)?.get(path)?.convert()
}

#[inline]
pub fn set<T: ToJson>(json: &str, path: &str, x: T) -> Result<String, Error> {
    let mut js = Json::parse(json)?;
    js.set(path, x)?;
    Ok(js.to_string())
}

#[inline]
pub fn del(json: &str, path: &str) -> Result<String, Error> {
    let mut js = Json::parse(json)?;
    js.del(path)?;
    Ok(js.to_string())
}

use ErrorKind::*;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub msg: String,
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", &self.kind, &self.msg)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    // parse error
    InvalidJson,
    ParseObjectError,
    ParseArrayError,
    ParseStringError,
    ParseNumberError,
    ParseBooleanError,
    ParseNullError,

    // operate error
    MissingKey,
    IndexOutOfRange,
    InvalidValueKind,
    PathSyntaxError,

    // convert error
    ConvertError,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}",
               match self {
                   InvalidJson => "InvalidJson",
                   ParseObjectError => "ParseObjectError",
                   ParseArrayError => "ParseArrayError",
                   ParseStringError => "ParseStringError",
                   ParseNumberError => "ParseNumberError",
                   ParseBooleanError => "ParseBooleanError",
                   ParseNullError => "ParseNullError",
                   MissingKey => "MissingKey",
                   IndexOutOfRange => "IndexOutOfRange",
                   InvalidValueKind => "InvalidValueKind",
                   PathSyntaxError => "PathSyntaxError",
                   ConvertError => "ConvertError",
               }
        )
    }
}

impl Error {
    pub fn new(kind: ErrorKind, msg: String) -> Error {
        Error { kind, msg }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Json {
    Object(HashMap<String, (Json, LinkedList<Json>)>),
    Array(Vec<Json>),
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

impl ToString for Json {
    fn to_string(&self) -> String {
        match self {
            Json::Object(obj) => {
                let mut obj = obj.iter().collect::<Vec<_>>();
                obj.sort_by(|(k1, (_, _)), (k2, (_, _))| k1.cmp(k2));
                format!("{{{}}}",
                        obj.iter()
                            .map(|(k, (js, list))| {
                                let mut s = String::with_capacity(4);
                                for js in list.iter().rev() {
                                    s.push_str(&format!("{}: {}, ", escape(k), js.to_string()));
                                }
                                s.push_str(&format!("{}: {}", escape(k), js.to_string()));
                                s
                            })
                            .collect::<Vec<_>>()
                            .join(", "))
            }
            Json::Array(vec) => {
                format!("[{}]",
                        vec.iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", "))
            }
            Json::String(s) => escape(s),
            Json::Number(f) => (*f).to_string(),
            Json::Boolean(b) => (*b).to_string(),
            Json::Null => "null".to_string(),
        }
    }
}

impl FromStr for Json {
    type Err = Error;
    fn from_str(s: &str) -> Result<Json, Error> {
        Json::parse(s)
    }
}

impl Json {
    pub fn insert<K: ToString, V: ToJson>(&mut self, k: K, v: V) -> Result<(), Error> {
        self.add_option(k.to_string(), v.to_json(), false)
    }

    pub fn append<K: ToString, V: ToJson>(&mut self, k: K, v: V) -> Result<(), Error> {
        self.add_option(k.to_string(), v.to_json(), true)
    }

    pub fn add_option(&mut self, k: String, v: Json, append: bool) -> Result<(), Error> {
        match self {
            Json::Object(obj) => {
                if append {
                    object_insert(obj, k, v);
                } else {
                    obj.insert(k, (v, LinkedList::new()));
                }
                Ok(())
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect object, got {}.", other._kind())))
        }
    }

    pub fn delete(&mut self, k: &str) -> Result<(), Error> {
        match self {
            Json::Object(obj) => {
                obj.remove(k);
                Ok(())
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect object, got {}.", other._kind())))
        }
    }

    pub fn push<E: ToJson>(&mut self, e: E) -> Result<(), Error> {
        match self {
            Json::Array(vec) => {
                vec.push(e.to_json());
                Ok(())
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect array, got {}.", other._kind())))
        }
    }

    pub fn replace<V: ToJson>(&mut self, i: usize, v: V) -> Result<(), Error> {
        match self {
            Json::Array(vec) => {
                match vec.get_mut(i) {
                    Some(js) => *js = v.to_json(),
                    None => return Err(Error::new(IndexOutOfRange,
                                                  format!("index {} out of range.", i)))
                }
                Ok(())
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect array, got {}.", other._kind())))
        }
    }

    pub fn remove(&mut self, i: usize) -> Result<(), Error> {
        match self {
            Json::Array(vec) => {
                vec.remove(i);
                Ok(())
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect array, got {}.", other._kind())))
        }
    }

    pub fn index_and_merge(&self, k: &str) -> Result<Vec<&Json>, Error> {
        match self {
            Json::Object(obj) => {
                match obj.get(k) {
                    Some((js, list)) => {
                        let mut vec = Vec::with_capacity(list.len() + 1);
                        vec.extend(list.iter().rev());
                        vec.push(js);
                        Ok(vec)
                    }
                    None => Err(Error::new(MissingKey,
                                           format!("missing key \"{}\".", k))),
                }
            }
            other => Err(Error::new(InvalidValueKind,
                                    format!("invalid value kind, expect object, got {}.", other._kind())))
        }
    }

    pub fn convert<T: FromJson>(&self) -> Result<T, Error> {
        T::from_json(self)
    }

    #[inline]
    pub fn _kind(&self) -> &'static str {
        match self {
            Json::Object(_) => "object",
            Json::Array(_) => "array",
            Json::String(_) => "string",
            Json::Number(_) => "number",
            Json::Boolean(_) => "boolean",
            Json::Null => "null",
        }
    }
}

impl Json {
    pub fn parse(json: &str) -> Result<Json, Error> {
        let json = json.trim_matches(is_whitespace);
        Json::_parse(json)
    }

    fn _parse(json: &str) -> Result<Json, Error> {
        match json.chars().peekable().peek() {
            Some('n') => Json::parse_null(json),
            Some('t') | Some('f') => Json::parse_boolean(json),
            Some('-') | Some('0'..='9') => Json::parse_number(json),
            Some('\"') => Json::parse_string(json),
            Some('[') => Json::parse_array(json),
            Some('{') => Json::parse_object(json),
            _ => Err(Error::new(InvalidJson, "invalid json.".to_string()))
        }
    }

    #[inline]
    fn parse_null(json: &str) -> Result<Json, Error> {
        if json == "null" {
            Ok(Json::Null)
        } else {
            Err(Error::new(ParseNullError, "error parsing null value.".to_string()))
        }
    }

    #[inline]
    fn parse_boolean(json: &str) -> Result<Json, Error> {
        match json {
            "true" => Ok(Json::Boolean(true)),
            "false" => Ok(Json::Boolean(false)),
            _ => Err(Error::new(ParseBooleanError, "error parsing boolean value.".to_string()))
        }
    }

    #[inline]
    fn parse_number(json: &str) -> Result<Json, Error> {
        if valid_number(json) {
            if let Ok(f) = json.parse::<f64>() {
                if f.is_finite() {
                    return Ok(Json::Number(f));
                } else {
                    return Err(Error::new(ParseNumberError, "error number too large.".to_string()));
                }
            } else {
                return Err(Error::new(ParseNumberError, "error parsing number value.".to_string()));
            }
        } else {
            return Err(Error::new(ParseNumberError, "invalid number.".to_string()));
        }
    }

    fn parse_string(json: &str) -> Result<Json, Error> {
        let mut s = String::new();
        let mut iter = json.chars().skip(1);
        loop {
            match iter.next() {
                Some('\"') => {
                    loop {
                        match iter.next() {
                            Some(ch) if is_whitespace(ch) => (),
                            Some(ch) => return Err(Error::new(ParseStringError,
                                                              format!("invalid trailing character: {}.", ch))),
                            None => break
                        }
                    }
                    return Ok(Json::String(s));
                }
                Some('\\') => {
                    match iter.next() {
                        Some('\"') => s.push('\"'),
                        Some('\\') => s.push('\\'),
                        Some('/') => s.push('/'),
                        Some('b') => s.push('\x08'),
                        Some('f') => s.push('\x0C'),
                        Some('n') => s.push('\n'),
                        Some('r') => s.push('\r'),
                        Some('t') => s.push('\t'),
                        Some('u') => {
                            let mut u = 0;
                            for _ in 0..4 {
                                match iter.next() {
                                    Some(ch) => u = (u << 4) | parse_hex_digit(ch)?,
                                    None => return Err(Error::new(ParseStringError,
                                                                  "parse unicode error: missing hex digit.".to_string()))
                                }
                            }

                            if u >= 0xD800 && u <= 0xDBFF {
                                match (iter.next(), iter.next()) {
                                    (Some('\\'), Some('u')) => (),
                                    _ => return Err(Error::new(ParseStringError,
                                                               "missing low surrogate.".to_string()))
                                }
                                let mut u2 = 0;
                                for _ in 0..4 {
                                    match iter.next() {
                                        Some(ch) => u2 = (u2 << 4) | parse_hex_digit(ch)?,
                                        None => return Err(Error::new(ParseStringError,
                                                                      "parse unicode error: missing hex digit.".to_string()))
                                    }
                                }
                                if u2 < 0xDC00 || u2 > 0xDFFF {
                                    return Err(Error::new(ParseStringError,
                                                          "invalid low surrogate.".to_string()));
                                }
                                u = (((u - 0xD800) << 10) | (u2 - 0xDC00)) + 0x10000;
                            }

                            if let Ok(unicode) = str::from_utf8(&encode_utf8(u)) {
                                s.push_str(unicode);
                            } else {
                                return Err(Error::new(ParseStringError,
                                                      "invalid utf-8 encoding.".to_string()));
                            }
                        }
                        _ => return Err(Error::new(ParseStringError,
                                                   "missing escaped character.".to_string()))
                    }
                }
                Some('\x00'..='\x1F') => return Err(Error::new(ParseStringError,
                                                               "invalid character.".to_string())),
                Some(ch) => s.push(ch),
                None => return Err(Error::new(ParseStringError, "missing \".".to_string()))
            }
        }
    }

    fn parse_array(json: &str) -> Result<Json, Error> {
        let mut array = Vec::new();
        let mut iter = json.chars().skip(1).peekable();
        if let Some(']') = iter.peek() {
            return Ok(Json::Array(array));
        }

        let mut i = 0;
        let mut j = 0;
        let mut inner = String::new();
        loop {
            match iter.next() {
                Some(']') if i == 0 && j == 0 => {
                    array.push(Json::_parse(&inner)?);
                    loop {
                        match iter.next() {
                            Some(ch) if is_whitespace(ch) => (),
                            Some(ch) => return Err(Error::new(ParseArrayError,
                                                              format!("invalid trailing character: {}.", ch))),
                            None => break
                        }
                    }
                    return Ok(Json::Array(array));
                }
                Some(',') if i == 0 && j == 0 => {
                    if inner.is_empty() {
                        return Err(Error::new(ParseArrayError, "invalid empty element.".to_string()));
                    }
                    if let Some(']') = iter.peek() {
                        return Err(Error::new(ParseArrayError, "invalid trailing comma.".to_string()));
                    }
                    array.push(Json::_parse(&inner)?);
                    inner.clear();
                }
                Some('\"') => {
                    if j <= 0 {
                        j += 1;
                    } else {
                        j -= 1;
                    }
                    inner.push('\"');
                }
                Some('[') => {
                    i += 1;
                    inner.push('[');
                }
                Some(']') => {
                    i -= 1;
                    inner.push(']');
                }
                Some('{') => {
                    i += 1;
                    inner.push('{');
                }
                Some('}') => {
                    i -= 1;
                    inner.push('}');
                }
                Some('\\') => {
                    inner.push('\\');
                    match iter.next() {
                        Some(ch) => inner.push(ch),
                        _ => return Err(Error::new(ParseArrayError,
                                                   "missing escaped character.".to_string()))
                    }
                }
                Some(ch) if i == 0 && j == 0 && is_whitespace(ch) => (),
                Some(ch) => {
                    inner.push(ch);
                }
                None => return Err(Error::new(ParseArrayError, "missing ].".to_string()))
            }
        }
    }

    fn parse_object(json: &str) -> Result<Json, Error> {
        let mut object = HashMap::new();
        let mut iter = json.chars().skip(1).peekable();
        if let Some('}') = iter.peek() {
            return Ok(Json::Object(object));
        }

        let mut i = 0;
        let mut j = 0;
        let mut k = String::new();
        let mut v = String::new();
        let mut on_key = true;
        loop {
            match iter.next() {
                Some('}') if i == 0 && j == 0 && !on_key => {
                    if let Ok(Json::String(key)) = Json::_parse(&k) {
                        object_insert(&mut object, key, Json::_parse(&v)?);
                        loop {
                            match iter.next() {
                                Some(ch) if is_whitespace(ch) => (),
                                Some(ch) => return Err(Error::new(ParseObjectError,
                                                                  format!("invalid trailing character: {}.", ch))),
                                None => break
                            }
                        }
                        return Ok(Json::Object(object));
                    } else {
                        return Err(Error::new(ParseObjectError, "error parsing key string.".to_string()));
                    }
                }
                Some(',') if i == 0 && j == 0 && !on_key => {
                    if k.is_empty() || v.is_empty() {
                        return Err(Error::new(ParseObjectError, "invalid empty member.".to_string()));
                    }
                    if let Some('}') = iter.peek() {
                        return Err(Error::new(ParseObjectError, "invalid trailing comma.".to_string()));
                    }
                    if let Ok(Json::String(key)) = Json::_parse(&k) {
                        object_insert(&mut object, key, Json::_parse(&v)?);
                        k.clear();
                        v.clear();
                        on_key = true;
                    } else {
                        return Err(Error::new(ParseObjectError, "error parsing key string.".to_string()));
                    }
                }
                Some(':') if i == 0 && j == 0 => {
                    on_key = false;
                }
                Some('\"') => {
                    if j <= 0 {
                        j += 1;
                    } else {
                        j -= 1;
                    }
                    if on_key {
                        k.push('\"');
                    } else {
                        v.push('\"');
                    }
                }
                Some('[') => {
                    i += 1;
                    if on_key {
                        k.push('[');
                    } else {
                        v.push('[');
                    }
                }
                Some(']') => {
                    i -= 1;
                    if on_key {
                        k.push(']');
                    } else {
                        v.push(']');
                    }
                }
                Some('{') => {
                    i += 1;
                    if on_key {
                        k.push('{');
                    } else {
                        v.push('{');
                    }
                }
                Some('}') => {
                    i -= 1;
                    if on_key {
                        k.push('}');
                    } else {
                        v.push('}');
                    }
                }
                Some('\\') => {
                    if on_key {
                        k.push('\\');
                    } else {
                        v.push('\\');
                    }
                    match iter.next() {
                        Some(ch) => {
                            if on_key {
                                k.push(ch);
                            } else {
                                v.push(ch);
                            }
                        }
                        _ => return Err(Error::new(ParseObjectError,
                                                   "missing escaped character.".to_string()))
                    }
                }
                Some(ch) if i == 0 && j == 0 && is_whitespace(ch) => (),
                Some(ch) => {
                    if on_key {
                        k.push(ch);
                    } else {
                        v.push(ch);
                    }
                }
                None => return Err(Error::new(ParseObjectError, "missing }.".to_string()))
            }
        }
    }
}

fn object_insert(object: &mut HashMap<String, (Json, LinkedList<Json>)>, key: String, mut json: Json) {
    match object.get_mut(&key) {
        Some((js, list)) => {
            mem::swap(js, &mut json);
            list.push_front(json);
        }
        None => { object.insert(key, (json, LinkedList::new())); }
    }
}

fn escape(input: &str) -> String {
    let mut output = String::with_capacity(input.len() + 2);
    output.push('\"');
    for ch in input.chars() {
        match ch {
            '\"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '/' => output.push_str("\\/"),
            '\x08' => output.push_str("\\b"),
            '\x0C' => output.push_str("\\f"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            '\x00'..='\x1F' => output.push_str(&hex(ch as usize)),
            '\x20'..='\x7E' => output.push(ch),
            ch => {
                let u = ch as usize;
                if u <= 0xFFFF {
                    output.push_str(&hex(u));
                } else {
                    let surrogate = u - 0x10000;
                    let (high, low) = ((surrogate >> 10) + 0xD800, (surrogate & 0b1111111111) + 0xDC00);
                    output.push_str(&hex(high));
                    output.push_str(&hex(low));
                }
            }
        }
    }
    output.push('\"');
    output
}

const HEX_TABLE: [char; 16] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

fn hex(mut u: usize) -> String {
    if u == 0 {
        return "\\u0000".to_string();
    }
    let mut vec = Vec::with_capacity(6);
    while u > 0 {
        vec.push(HEX_TABLE[u & 0b1111]);
        u >>= 4;
    }
    if vec.len() < 4 {
        for _ in vec.len()..4 {
            vec.push('0');
        }
    }
    vec.push('u');
    vec.push('\\');
    vec.reverse();
    vec.into_iter().collect()
}

fn parse_hex_digit(ch: char) -> Result<usize, Error> {
    let u = ch as usize;
    if u >= '0' as usize && u <= '9' as usize {
        Ok(u - '0' as usize)
    } else if u >= 'A' as usize && u <= 'F' as usize {
        Ok(u - 'A' as usize + 10)
    } else if u >= 'a' as usize && u <= 'f' as usize {
        Ok(u - 'a' as usize + 10)
    } else {
        Err(Error::new(ParseStringError,
                       format!("parse unicode error: invalid hex digit {}.", ch)))
    }
}

fn encode_utf8(u: usize) -> Vec<u8> {
    let mut v: Vec<u8> = Vec::with_capacity(4);
    if u <= 0x7F {
        v.push(u as u8);
    } else if u <= 0x7FF {
        v.push((0xC0 | (u >> 6)) as u8);
        v.push((0x80 | (u & 0x3F)) as u8);
    } else if u <= 0xFFFF {
        v.push((0xE0 | (u >> 12)) as u8);
        v.push((0x80 | ((u >> 6) & 0x3F)) as u8);
        v.push((0x80 | (u & 0x3F)) as u8);
    } else {
        v.push((0xF0 | (u >> 18)) as u8);
        v.push((0x80 | ((u >> 12) & 0x3F)) as u8);
        v.push((0x80 | ((u >> 6) & 0x3F)) as u8);
        v.push((0x80 | (u & 0x3F)) as u8);
    }
    v
}

fn valid_number(num: &str) -> bool {
    let mut iter = num.chars().peekable();

    // validate '-'
    match iter.peek() {
        Some('-') => { iter.next(); }
        _ => ()
    }

    // validate '0'/'1'~'9'
    match iter.peek() {
        Some('0') => { iter.next(); }
        Some('1'..='9') => {
            loop {
                match iter.peek() {
                    Some('0'..='9') => { iter.next(); }
                    _ => break
                }
            }
        }
        _ => return false
    }

    // validate '.'
    match iter.peek() {
        Some('.') => {
            match iter.nth(1) {
                Some('0'..='9') => (),
                _ => return false
            }
            loop {
                match iter.peek() {
                    Some('0'..='9') => { iter.next(); }
                    _ => break
                }
            }
        }
        _ => ()
    }

    // validate 'e'/'E'
    match iter.peek() {
        Some('e') | Some('E') => {
            iter.next();
            match iter.peek() {
                Some('+') | Some('-') => { iter.next(); }
                _ => ()
            }
            loop {
                match iter.peek() {
                    Some('0'..='9') => { iter.next(); }
                    _ => break
                }
            }
        }
        _ => ()
    }

    true
}

#[inline]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

pub trait ToJson {
    fn to_json(&self) -> Json;
}

pub trait FromJson: Sized {
    fn from_json(json: &Json) -> Result<Self, Error>;
}

impl ToJson for Json {
    fn to_json(&self) -> Json {
        self.clone()
    }
}

impl FromJson for Json {
    fn from_json(json: &Json) -> Result<Self, Error> {
        Ok(json.clone())
    }
}

impl ToJson for String {
    fn to_json(&self) -> Json {
        Json::String(self.clone())
    }
}

impl FromJson for String {
    fn from_json(json: &Json) -> Result<String, Error> {
        match json {
            Json::String(s) => Ok(s.clone()),
            other => Err(Error::new(ConvertError,
                                    format!("cannot convert {} to String.", other._kind())))
        }
    }
}

impl ToJson for str {
    fn to_json(&self) -> Json {
        Json::String(self.to_string())
    }
}

impl ToJson for &str {
    fn to_json(&self) -> Json {
        Json::String(self.to_string())
    }
}

macro_rules! number_imp {
    ( $ty:ty ) => {
        impl ToJson for $ty {
            fn to_json(&self) -> Json {
                Json::Number(*self as f64)
            }
        }

        impl FromJson for $ty {
            fn from_json(json: &Json) -> Result<$ty, Error> {
                match json {
                    Json::Number(n) => Ok(*n as $ty),
                    other => Err(Error::new(ConvertError,
                                            format!("cannot convert {} to {}.", other._kind(), stringify!($ty))))
                }
            }
        }

        impl ToJson for &$ty {
            fn to_json(&self) -> Json {
                Json::Number(**self as f64)
            }
        }
    };
}

number_imp!(u8);
number_imp!(u16);
number_imp!(u32);
number_imp!(u64);
number_imp!(u128);
number_imp!(usize);
number_imp!(i8);
number_imp!(i16);
number_imp!(i32);
number_imp!(i64);
number_imp!(i128);
number_imp!(isize);
number_imp!(f32);
number_imp!(f64);

impl ToJson for bool {
    fn to_json(&self) -> Json {
        Json::Boolean(*self)
    }
}

impl FromJson for bool {
    fn from_json(json: &Json) -> Result<bool, Error> {
        match json {
            Json::Boolean(b) => Ok(*b),
            other => Err(Error::new(ConvertError, format!("cannot convert {} to bool.", other._kind())))
        }
    }
}

impl ToJson for &bool {
    fn to_json(&self) -> Json {
        Json::Boolean(**self)
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    fn to_json(&self) -> Json {
        Json::Array(self.iter().map(|x| x.to_json()).collect())
    }
}

impl<T: FromJson> FromJson for Vec<T> {
    fn from_json(json: &Json) -> Result<Vec<T>, Error> {
        match json {
            Json::Array(v) => {
                Ok(v.iter().map(|x| T::from_json(x)).collect::<Result<Vec<T>, Error>>()?)
            }
            other => Err(Error::new(ConvertError, format!("cannot convert {} to Vec", other._kind())))
        }
    }
}

impl<T: ToJson> ToJson for &[T] {
    fn to_json(&self) -> Json {
        Json::Array(self.iter().map(|x| x.to_json()).collect())
    }
}

impl<K: ToString, V: ToJson> ToJson for Vec<(K, V)> {
    fn to_json(&self) -> Json {
        let mut object = HashMap::with_capacity(self.len());
        for (k, v) in self {
            object_insert(&mut object, k.to_string(), v.to_json());
        }
        Json::Object(object)
    }
}

impl<K: FromStr, V: FromJson> FromJson for Vec<(K, V)> {
    fn from_json(json: &Json) -> Result<Vec<(K, V)>, Error> {
        match json {
            Json::Object(obj) => {
                let mut vec = Vec::with_capacity(obj.len());
                for (mk, (mj, mv)) in obj {
                    for mj in mv.iter().rev() {
                        let k = match K::from_str(mk) {
                            Ok(k) => k,
                            Err(_) => return Err(Error::new(ConvertError,
                                                            format!("error converting {} to key.", mk)))
                        };
                        let v = V::from_json(mj)?;
                        vec.push((k, v));
                    }
                    let k = match K::from_str(mk) {
                        Ok(k) => k,
                        Err(_) => return Err(Error::new(ConvertError,
                                                        format!("error converting {} to key.", mk)))
                    };
                    let v = V::from_json(mj)?;
                    vec.push((k, v));
                }
                Ok(vec)
            }
            other => Err(Error::new(ConvertError,
                                    format!("cannot convert {} to Vec<(K, V)>", other._kind())))
        }
    }
}

impl<K: ToString, V: ToJson> ToJson for &[(K, V)] {
    fn to_json(&self) -> Json {
        let mut object = HashMap::with_capacity(self.len());
        for (k, v) in self.iter() {
            object_insert(&mut object, k.to_string(), v.to_json());
        }
        Json::Object(object)
    }
}

impl<K: Hash + Eq + ToString, V: ToJson> ToJson for HashMap<K, V> {
    fn to_json(&self) -> Json {
        let mut object = HashMap::with_capacity(self.len());
        for (k, v) in self.iter() {
            object.insert(k.to_string(), (v.to_json(), LinkedList::new()));
        }
        Json::Object(object)
    }
}

impl<K: Hash + Eq + FromStr, V: FromJson> FromJson for HashMap<K, V> {
    fn from_json(json: &Json) -> Result<HashMap<K, V>, Error> {
        match json {
            Json::Object(obj) => {
                let mut map = HashMap::with_capacity(obj.len());
                for (mk, (mj, _)) in obj {
                    let k = match K::from_str(mk) {
                        Ok(k) => k,
                        Err(_) => return Err(Error::new(ConvertError,
                                                        format!("error converting {} to key.", mk)))
                    };
                    let v = V::from_json(mj)?;
                    map.insert(k, v);
                }
                Ok(map)
            }
            other => Err(Error::new(ConvertError,
                                    format!("cannot convert {} to HashMap", other._kind())))
        }
    }
}

impl<T: ToJson> ToJson for Option<T> {
    fn to_json(&self) -> Json {
        match self {
            Some(some) => some.to_json(),
            None => Json::Null,
        }
    }
}

impl<T: FromJson> FromJson for Option<T> {
    fn from_json(json: &Json) -> Result<Option<T>, Error> {
        match json {
            Json::Null => Ok(None),
            other => Ok(Some(T::from_json(other)?)),
        }
    }
}

#[macro_export]
macro_rules! object {
    ( $( $k:expr => $v:expr ),* $(,)?) => {{
        let _obj: std::collections::HashMap<String, ($crate::Json, std::collections::LinkedList<$crate::Json>)> = std::collections::HashMap::new();
        let mut _json = $crate::Json::Object(_obj);
        $(let _ = _json.append($k, $v);)*
        _json
    }};
}

#[macro_export]
macro_rules! array {
    ( $( $e:expr ),* $(,)?) => {{
        let mut _vec: Vec<$crate::Json> = Vec::new();
        $(_vec.push($crate::ToJson::to_json(&$e));)*
        $crate::Json::Array(_vec)
    }};
}

#[derive(Debug)]
pub struct Path {
    finder: Finder,
    next: Option<Box<Path>>,
}

impl Path {
    pub fn new<F: Into<Finder>>(f: F) -> Path {
        Path { finder: f.into(), next: None }
    }

    pub fn append<F: Into<Finder>>(&mut self, f: F) -> &mut Path {
        match &mut self.next {
            Some(path) => { path.append(f); }
            None => self.next = Some(Box::new(Path::new(f))),
        }
        self
    }

    fn find<'a>(&self, json: &'a Json) -> Result<&'a Json, Error> {
        match self.finder.find(json) {
            Ok(inner) => {
                match &self.next {
                    Some(path) => path.find(inner),
                    None => Ok(inner),
                }
            }
            err @ Err(_) => err,
        }
    }

    fn find_mut<'a>(&self, json: &'a mut Json) -> Result<&'a mut Json, Error> {
        match self.finder.find_mut(json) {
            Ok(inner) => {
                match &self.next {
                    Some(path) => path.find_mut(inner),
                    None => Ok(inner),
                }
            }
            err @ Err(_) => err,
        }
    }

    fn discard(&self, json: &mut Json) -> Result<Json, Error> {
        match &self.next {
            Some(path) => {
                match self.finder.find_mut(json) {
                    Ok(inner) => path.discard(inner),
                    Err(err) => Err(err)
                }
            }
            None => self.finder.discard(json)
        }
    }

    fn update<'a, V: ToJson>(&self, json: &'a mut Json, v: V) -> Result<&'a mut Json, Error> {
        match &self.next {
            Some(path) => {
                if self.finder.find(json).is_ok() {
                    let inner = self.finder.find_mut(json)?;
                    path.update(inner, v)
                } else {
                    let inner = self.finder.update(json, Json::Null)?;
                    path.update(inner, v)
                }
            }
            None => self.finder.update(json, v.to_json()),
        }
    }
}

impl Json {
    pub fn find<I: Into<Path>>(&self, path: I) -> Result<&Json, Error> {
        path.into().find(self)
    }

    pub fn find_mut<I: Into<Path>>(&mut self, path: I) -> Result<&mut Json, Error> {
        path.into().find_mut(self)
    }

    pub fn discard<I: Into<Path>>(&mut self, path: I) -> Result<Json, Error> {
        path.into().discard(self)
    }

    pub fn update<I: Into<Path>, V: ToJson>(&mut self, path: I, v: V) -> Result<&mut Json, Error> {
        path.into().update(self, v)
    }

    pub fn get(&self, s: &str) -> Result<&Json, Error> {
        if s.is_empty() {
            return Err(Error::new(PathSyntaxError, "invalid syntax: empty path.".to_string()));
        }
        parse_path(s)?.find(self)
    }

    pub fn del(&mut self, s: &str) -> Result<Json, Error> {
        if s.is_empty() {
            return Err(Error::new(PathSyntaxError, "invalid syntax: empty path.".to_string()));
        }
        parse_path(s)?.discard(self)
    }

    pub fn set<V: ToJson>(&mut self, s: &str, v: V) -> Result<&mut Json, Error> {
        if s.is_empty() {
            return Err(Error::new(PathSyntaxError, "invalid syntax: empty path.".to_string()));
        }
        parse_path(s)?.update(self, v)
    }
}

fn parse_path(s: &str) -> Result<Path, Error> {
    let mut iter = s.chars().peekable();
    let mut buf = String::new();
    let mut vec: Vec<Finder> = Vec::new();

    let f = |buf: &mut String| {
        if buf.starts_with('#') {
            let s = buf.index(1..);
            let i = match s.parse::<usize>() {
                Ok(i) => i,
                Err(_) => return Err(Error::new(PathSyntaxError,
                                                format!("error parsing \"{}\" to usize.", s))),
            };
            let f = Finder::from(i);
            buf.clear();
            Ok(f)
        } else if buf.starts_with('\\') {
            let f = Finder::from(buf.index(1..));
            buf.clear();
            Ok(f)
        } else {
            let f = Finder::from(buf as &String);
            buf.clear();
            Ok(f)
        }
    };

    loop {
        match iter.next() {
            Some('.') => vec.push(f(&mut buf)?),
            Some('\\') => {
                match iter.peek() {
                    Some('.') => {
                        buf.push('.');
                        iter.next();
                    }
                    _ => buf.push('\\'),
                }
            }
            Some(ch) => buf.push(ch),
            _ => {
                vec.push(f(&mut buf)?);
                break;
            }
        }
    }

    let mut iter = vec.into_iter();
    let mut path = Path::new(
        iter.next().ok_or(Error::new(PathSyntaxError, "invalid syntax: empty path.".to_string()))?
    );
    iter.for_each(|f| { path.append(f); });

    Ok(path)
}

#[derive(Debug)]
pub enum Finder {
    Key(String),
    Index(usize),
}

impl Finder {
    fn find<'a>(&self, json: &'a Json) -> Result<&'a Json, Error> {
        match self {
            Finder::Key(k) => {
                match json {
                    Json::Object(obj) => {
                        match obj.get(k) {
                            Some((js, _)) => Ok(js),
                            None => Err(Error::new(MissingKey,
                                                   format!("missing key \"{}\".", k))),
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by key \"{}\" error: invalid value kind, expect object, got {}.", k, other._kind())))
                    }
                }
            }
            Finder::Index(i) => {
                match json {
                    Json::Array(vec) => {
                        match vec.get(*i) {
                            Some(js) => Ok(js),
                            None => Err(Error::new(IndexOutOfRange,
                                                   format!("index {} out of range.", *i))),
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by index {} error: invalid value kind, expect array, got {}.", *i, other._kind())))
                    }
                }
            }
        }
    }

    fn find_mut<'a>(&self, json: &'a mut Json) -> Result<&'a mut Json, Error> {
        match self {
            Finder::Key(k) => {
                match json {
                    Json::Object(obj) => {
                        match obj.get_mut(k) {
                            Some((js, _)) => Ok(js),
                            None => Err(Error::new(MissingKey,
                                                   format!("missing key \"{}\".", k))),
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by key \"{}\" error: invalid value kind, expect object, got {}.", k, other._kind())))
                    }
                }
            }
            Finder::Index(i) => {
                match json {
                    Json::Array(vec) => {
                        match vec.get_mut(*i) {
                            Some(js) => Ok(js),
                            None => Err(Error::new(IndexOutOfRange,
                                                   format!("index {} out of range.", *i))),
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by index {} error: invalid value kind, expect array, got {}.", *i, other._kind())))
                    }
                }
            }
        }
    }

    fn discard(&self, json: &mut Json) -> Result<Json, Error> {
        match self {
            Finder::Key(k) => {
                match json {
                    Json::Object(obj) => {
                        match obj.remove(k) {
                            Some((js, _)) => Ok(js),
                            None => Err(Error::new(MissingKey,
                                                   format!("missing key \"{}\".", k))),
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by key \"{}\" error: invalid value kind, expect object, got {}.", k, other._kind())))
                    }
                }
            }
            Finder::Index(i) => {
                match json {
                    Json::Array(vec) => {
                        if *i < vec.len() {
                            Ok(vec.remove(*i))
                        } else {
                            Err(Error::new(IndexOutOfRange,
                                           format!("index {} out of range.", *i)))
                        }
                    }
                    other => {
                        Err(Error::new(InvalidValueKind,
                                       format!("find value by index {} error: invalid value kind, expect array, got {}.", *i, other._kind())))
                    }
                }
            }
        }
    }

    fn update<'a>(&self, json: &'a mut Json, v: Json) -> Result<&'a mut Json, Error> {
        match self {
            Finder::Key(k) => {
                match json {
                    Json::Object(obj) => {
                        obj.insert(k.clone(), (v, LinkedList::new()));
                    }
                    _ => {
                        let mut obj = HashMap::with_capacity(1);
                        obj.insert(k.clone(), (v, LinkedList::new()));
                        *json = Json::Object(obj);
                    }
                }
                Ok(json.index_mut(k.as_str()))
            }
            Finder::Index(i) => {
                match json {
                    Json::Array(vec) => {
                        if *i >= vec.len() {
                            vec.resize(*i + 1, Json::Null);
                        }
                    }
                    _ => {
                        let mut vec = Vec::with_capacity(*i + 1);
                        vec.resize(*i + 1, Json::Null);
                        *json = Json::Array(vec);
                    }
                }
                let js = json.index_mut(*i);
                *js = v;
                Ok(js)
            }
        }
    }
}

impl From<String> for Finder {
    fn from(s: String) -> Finder {
        Finder::Key(s)
    }
}

impl From<&String> for Finder {
    fn from(s: &String) -> Finder {
        Finder::Key(s.clone())
    }
}

impl From<&str> for Finder {
    fn from(s: &str) -> Finder {
        Finder::Key(s.to_string())
    }
}

impl From<usize> for Finder {
    fn from(i: usize) -> Finder {
        Finder::Index(i)
    }
}

impl From<&usize> for Finder {
    fn from(i: &usize) -> Finder {
        Finder::Index(*i)
    }
}

impl<F: Into<Finder>> From<F> for Path {
    fn from(f: F) -> Path {
        Path::new(f)
    }
}

#[macro_export]
macro_rules! path {
    ( $root:expr $(, $next:expr)* $(,)? ) => {{
        let mut _path = $crate::Path::new($root);
        $(_path.append($next);)*
        _path
    }};
}

#[macro_export]
macro_rules! find {
    ( $json:expr, $root:expr $(, $next:expr)* $(,)? ) => {{
        let mut _path = $crate::Path::new($root);
        $(_path.append($next);)*
        $json.find(_path)
    }};
}

#[macro_export]
macro_rules! update {
    ( $expr:expr => $json:expr, $root:expr $(, $next:expr)* $(,)? ) => {{
        let mut _path = $crate::Path::new($root);
        $(_path.append($next);)*
        $json.update(_path, $expr)
    }};
}

#[macro_export]
macro_rules! discard {
    ( $json:expr, $root:expr $(, $next:expr)* $(,)? ) => {{
        let mut _path = $crate::Path::new($root);
        $(_path.append($next);)*
        $json.discard(_path)
    }};
}

impl<T: Into<Finder>> Index<T> for Json {
    type Output = Json;
    fn index(&self, index: T) -> &Json {
        Path::new(index).find(self).expect("Index")
    }
}

impl<T: Into<Finder>> IndexMut<T> for Json {
    fn index_mut(&mut self, index: T) -> &mut Json {
        Path::new(index).find_mut(self).expect("IndexMut")
    }
}

////////////////
// test cases //
////////////////

#[cfg(test)]
mod tests {
    use crate::*;

    macro_rules! ok {
        ( $ty:ident($n:expr), $s:literal ) => {
            assert! {
                match Json::parse($s) {
                    Ok(Json::$ty(n)) => {assert_eq!(n, $n); true},
                    _ => false
                }
            }
        };
        ( $ty:ident($n:expr), $json:expr ) => {
            assert! {
                match $json {
                    Json::$ty(n) => {assert_eq!(n, $n); true},
                    _ => false
                }
            }
        };
        ( $ty:ident, $s:literal ) => {
            assert! {
                match Json::parse($s) {
                    Ok(Json::$ty) => true,
                    _ => false
                }
            }
        };
        ( $ty:ident, $json:expr ) => {
            assert! {
                match $json {
                    Json::$ty => true,
                    _ => false
                }
            }
        };
    }

    macro_rules! invalid {
        ( $s:literal ) => {
            assert! {
                if let Err(_) = Json::parse($s) {
                    true
                } else {
                    false
                }
            }
        };
    }

    #[test]
    fn test_parse_null() {
        ok!(Null, "null");
        ok!(Null, " \t\n\rnull\n\r \t");
    }

    #[test]
    fn test_parse_boolean() {
        ok!(Boolean(true), "true");
        ok!(Boolean(false), "false");
        ok!(Boolean(true), " \t\n\rtrue\n\r \t");
        ok!(Boolean(false), " \t\n\rfalse\n\r \t");
    }

    #[test]
    fn test_parse_number() {
        ok!(Number(0.0), "0");
        ok!(Number(0.0), "-0");
        ok!(Number(0.0), "-0.0");
        ok!(Number(1.0), "1");
        ok!(Number(-1.0), "-1");
        ok!(Number(1.5), "1.5");
        ok!(Number(-1.5), "-1.5");
        ok!(Number(3.1416), "3.1416");
        ok!(Number(1E10), "1E10");
        ok!(Number(1e10), "1e10");
        ok!(Number(1E+10), "1E+10");
        ok!(Number(1E-10), "1E-10");
        ok!(Number(-1E10), "-1E10");
        ok!(Number(-1e10), "-1e10");
        ok!(Number(-1E+10), "-1E+10");
        ok!(Number(-1E-10), "-1E-10");
        ok!(Number(1.234E+10), "1.234E+10");
        ok!(Number(1.234E-10), "1.234E-10");
        ok!(Number(0.0), "1e-10000");

        ok!(Number( 1.0000000000000002), "1.0000000000000002");
        ok!(Number( 4.9406564584124654e-324), "4.9406564584124654e-324");
        ok!(Number(-4.9406564584124654e-324), "-4.9406564584124654e-324");
        ok!(Number( 2.2250738585072009e-308), "2.2250738585072009e-308");
        ok!(Number(-2.2250738585072009e-308), "-2.2250738585072009e-308");
        ok!(Number( 2.2250738585072014e-308), "2.2250738585072014e-308");
        ok!(Number(-2.2250738585072014e-308), "-2.2250738585072014e-308");
        ok!(Number( 1.7976931348623157e+308), "1.7976931348623157e+308");
        ok!(Number(-1.7976931348623157e+308), "-1.7976931348623157e+308");

        invalid!("0  +");
        invalid!("+0");
        invalid!("+1");
        invalid!(".123");
        invalid!("1.");
        invalid!("INF");
        invalid!("inf");
        invalid!("NAN");
        invalid!("nan");
        invalid!("1e309");
        invalid!("-1e309");
    }

    #[test]
    fn test_parse_string() {
        ok!(String("123"), "\"123\"");
        ok!(String("12\n3"), "\"12\\n3\"");
        ok!(String("测试"), "\"测试\"");
        ok!(String("一二三"), "\"\\u4e00\\u4e8c\\u4e09\"");
        ok!(String("💖"), "\"💖\"");
        ok!(String("💖\x00"), "\"\\ud83d\\udc96\\u0000\"");

        invalid!("\"123\" + ");
        invalid!("\"\\ud83d+\\udc96\"");

        assert_eq!(Json::parse("\"\\ud83d\\udc96\"").unwrap().to_string(), "\"\\uD83D\\uDC96\"");
    }

    #[test]
    fn test_parse_array() {
        assert! {
            if let Ok(Json::Array(_)) = Json::parse("[]") {
                true
            } else {
                false
            }
        }
        assert! {
            if let Ok(Json::Array(vec)) = Json::parse("\n[null,\n true,\t 1.234E+10 \r, \"\\ud83d\\udc96 \"  , {\"123\": 123}, [1, {}]]") {
                ok!(Null, vec[0]);
                ok!(Boolean(true), vec[1]);
                ok!(Number(1.234E+10), vec[2]);
                ok!(String("💖 "), &vec[3]);
                if let Json::Object(obj) = &vec[4] {
                    let (js, _) = obj.get("123").unwrap();
                    ok!(Number(&123f64), js);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }

        invalid!("[1, 2, \"123\"+]");
        invalid!("[1, 2+]");
        invalid!("[123,]");
        invalid!("[123,,123]")
    }

    #[test]
    fn test_parse_object() {
        assert! {
            if let Ok(Json::Object(_)) = Json::parse("{}") {
                true
            } else {
                false
            }
        }
        assert! {
            if let Ok(Json::Object(obj)) = Json::parse("{\"number\": 1.234E+10}") {
                let (js, _) = obj.get("number").unwrap();
                ok!(Number(&1.234E+10), js);
                true
            } else {
                false
            }
        }
        assert! {
            if let Ok(Json::Object(obj)) = Json::parse("{\"null\": null}") {
                let (js, _) = obj.get("null").unwrap();
                ok!(Null, js);
                true
            } else {
                false
            }
        }
        assert! {
            if let Ok(Json::Object(_)) = Json::parse("{\"first\": \"Dale\", \"last\": \"Murphy\", \"age\": 44, \"nets\": [\"ig\", \"fb\", \"tw\"]}") {
                true
            } else {
                false
            }
        }

        invalid!("{\"123\"}");
        invalid!("{\"123\": 123,}");
        invalid!("{\"123\":}");
        invalid!("{:123}");
    }

    static JSON: &'static str = "{\
        \"name\": {\
            \"first\": \"Tom\", \
            \"last\": \"Anderson\"\
        }, \
        \"age\": 37, \
        \"children\": [\"Sara\", \"Alex\", \"Jack\"], \
        \"fav.movie\": \"Deer Hunter\", \
        \"friends\": [\
            {\"first\": \"#Dale\", \"last\": \"Murphy\", \"age\": 44, \"nets\": [\"ig\", \"fb\", \"tw\"]}, \
            {\"first\": \"\\\\#Roger\", \"last\": \"Craig\", \"age\": 68, \"nets\": [\"fb\", \"tw\"]}, \
            {\"first\": \"Jane\", \"last\": \"\\\\Murphy\", \"age\": 47, \"nets\": [\"ig\", \"tw\"]}\
        ]\
    }";

    #[test]
    fn test_get() {
        let json = Json::parse(JSON).unwrap();

        ok!(String("Tom"), json.get("name.first").unwrap());
        ok!(String("Sara"), json.get("children.#0").unwrap());
        ok!(String("Deer Hunter"), json.get("fav\\.movie").unwrap());
        ok!(String("\\Murphy"), json.get("friends.#2.last").unwrap());
        ok!(String("#Dale"), json.get("friends.#0.first").unwrap());
        ok!(String("\\#Roger"), json.get("friends.#1.first").unwrap());
        ok!(String("fb"), json.get("friends.#0.nets.#1").unwrap());
        ok!(Number(&37f64), json.get("age").unwrap());
        ok!(Number(&68f64), json.get("friends.#1.age").unwrap());
    }

    #[test]
    fn test_set() {
        macro_rules! sassert {
            ( $json:ident, $s:literal, $ty:ident($v:expr) ) => {
                let v = $v;
                ok!($ty(v), $json.set($s, v).unwrap());
                ok!($ty(v), $json.get($s).unwrap());
            };
        }

        let mut json = Json::parse(JSON).unwrap();

        sassert!(json, "age", Number(&24f64));
        sassert!(json, "name.last", String("Rogers"));
        sassert!(json, "children.#4", String("Steve"));
        sassert!(json, "fav\\.movie", String("Spider Man"));
        sassert!(json, "friends.#0.nets.#3", String("fb"));
        sassert!(json, "phone", String("iPhone X"));

        ok!(Null, json.get("children.#3").unwrap());
    }

    #[test]
    fn test_del() {
        macro_rules! dassert {
            ( $json:ident, $s:literal ) => {
                let l = $json.get($s).unwrap().clone();
                let r = $json.del($s).unwrap();
                assert_eq!(l, r);
                assert!($json.get($s).is_err());
            };
        }

        let mut json = Json::parse(JSON).unwrap();

        dassert!(json, "name.first");
        dassert!(json, "fav\\.movie");
        dassert!(json, "age");
    }

    #[test]
    fn test_insert() {
        macro_rules! iassert {
            ( $json:ident, $ty:ident($v:expr), $s:literal, $len:literal ) => {
                ok!($ty($v), &$json[$s]);
                assert! {
                    match $json {
                        Json::Object(ref obj) => obj.iter().fold(0, |n, (_, (_, list))| n + 1 + list.len()) == $len,
                        _ => false,
                    }
                }
            };
        }

        let mut js = object! {
            "name" => "Jack",
            "name" => "Steve",
        };

        iassert!(js, String("Steve"), "name", 2);

        assert!(js.insert("name", "Tony").is_ok());
        iassert!(js, String("Tony"), "name", 1);

        assert!(js.append("name", "Peter").is_ok());
        iassert!(js, String("Peter"), "name", 2);

        for (js, name) in js.index_and_merge("name").unwrap().into_iter().zip(vec!["Tony", "Peter"]) {
            ok!(String(name), js);
        }
    }

    #[test]
    fn test_duplicate_order() {
        use std::ops::Index;
        use std::collections::HashMap;

        let js = object! {
            "name" => "Jack",
            "name" => "Steve",
        };

        ok!(String("Steve"), js.index("name"));

        let v = <Vec<(String, String)>>::from_json(&js).unwrap();
        assert_eq!(v.last(), Some(&("name".to_string(), "Steve".to_string())));
        ok!(String("Steve"), v.to_json().index("name"));

        let m = <HashMap<String, String>>::from_json(&js).unwrap();
        assert_eq!(m.get("name"), Some(&"Steve".to_string()));
        ok!(String("Steve"), m.to_json().index("name"));

        assert_eq!(crate::encode(&js), r#"{"name": "Jack", "name": "Steve"}"#);
        assert_eq!(crate::encode(&v), r#"{"name": "Jack", "name": "Steve"}"#);
        assert_eq!(crate::encode(&m), r#"{"name": "Steve"}"#);
    }

    #[test]
    fn test_direct_set_get_del() {
        let js = r#"{"name": "Rogers", "age": 24, "master": true, "phones": ["iPhone", "Pixel"]}"#;

        assert_eq!(crate::get::<String>(&js, "name").unwrap(), "Rogers".to_string());
        assert_eq!(crate::get::<i32>(&js, "age").unwrap(), 24);
        assert_eq!(crate::get::<bool>(&js, "master").unwrap(), true);
        assert_eq!(crate::get::<String>(&js, "phones.#0").unwrap(), "iPhone".to_string());

        assert_eq!(
            crate::set(&js, "name", "Steve").unwrap(),
            r#"{"age": 24, "master": true, "name": "Steve", "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::set(&js, "age", 33).unwrap(),
            r#"{"age": 33, "master": true, "name": "Rogers", "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::set(&js, "master", false).unwrap(),
            r#"{"age": 24, "master": false, "name": "Rogers", "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::set(&js, "phones.#2", "Mate").unwrap(),
            r#"{"age": 24, "master": true, "name": "Rogers", "phones": ["iPhone", "Pixel", "Mate"]}"#,
        );

        assert_eq!(
            crate::del(&js, "name").unwrap(),
            r#"{"age": 24, "master": true, "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::del(&js, "age").unwrap(),
            r#"{"master": true, "name": "Rogers", "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::del(&js, "master").unwrap(),
            r#"{"age": 24, "name": "Rogers", "phones": ["iPhone", "Pixel"]}"#,
        );
        assert_eq!(
            crate::del(&js, "phones.#0").unwrap(),
            r#"{"age": 24, "master": true, "name": "Rogers", "phones": ["Pixel"]}"#
        );
    }

    #[test]
    fn test_encode_decode() {
        assert_eq!(
            crate::encode("\" \\ / \x08 \x0C \n \r \t"),
            "\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"".to_string(),
        );
        assert_eq!(
            crate::decode::<String>("\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"").unwrap(),
            "\" \\ / \x08 \x0C \n \r \t".to_string(),
        );
    }

    #[test]
    fn test_macros() {
        let mut js = object! {
            "name" => "Rogers",
            "age" => 24,
            "master" => true,
            "phones" => array![
                "iPhone",
                "Pixel",
            ],
        };

        assert_eq!(
            js.to_string(),
            r#"{"age": 24, "master": true, "name": "Rogers", "phones": ["iPhone", "Pixel"]}"#,
        );

        assert_eq!(find!(js, "name").unwrap().convert::<String>().unwrap(), "Rogers".to_string());
        assert_eq!(find!(js, "age").unwrap().convert::<i32>().unwrap(), 24);
        assert_eq!(find!(js, "master").unwrap().convert::<bool>().unwrap(), true);
        assert_eq!(find!(js, "phones", 0).unwrap().convert::<String>().unwrap(), "iPhone".to_string());

        assert_eq!(js.find(path!("name")).unwrap().convert::<String>().unwrap(), "Rogers".to_string());
        assert_eq!(js.find(path!("age")).unwrap().convert::<i32>().unwrap(), 24);
        assert_eq!(js.find(path!("master")).unwrap().convert::<bool>().unwrap(), true);
        assert_eq!(js.find(path!("phones", 0)).unwrap().convert::<String>().unwrap(), "iPhone".to_string());

        ok!(String("Steve"), update!("Steve" => js, "name").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"age": 24, "master": true, "name": "Steve", "phones": ["iPhone", "Pixel"]}"#,
        );
        ok!(Number(&33f64), update!(33 => js, "age").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"age": 33, "master": true, "name": "Steve", "phones": ["iPhone", "Pixel"]}"#,
        );
        ok!(Boolean(&false), update!(false => js, "master").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"age": 33, "master": false, "name": "Steve", "phones": ["iPhone", "Pixel"]}"#,
        );
        ok!(String("Mate"), update!("Mate" => js, "phones", 2).unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"age": 33, "master": false, "name": "Steve", "phones": ["iPhone", "Pixel", "Mate"]}"#,
        );

        ok!(String("Steve"), discard!(js, "name").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"age": 33, "master": false, "phones": ["iPhone", "Pixel", "Mate"]}"#,
        );
        ok!(Number(33f64), discard!(js, "age").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"master": false, "phones": ["iPhone", "Pixel", "Mate"]}"#,
        );
        ok!(Boolean(false), discard!(js, "master").unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"phones": ["iPhone", "Pixel", "Mate"]}"#,
        );
        ok!(String("iPhone"), discard!(js, "phones", 0).unwrap());
        assert_eq!(
            js.to_string(),
            r#"{"phones": ["Pixel", "Mate"]}"#,
        );
    }

    #[test]
    fn test_operation() {
        let mut js = object!("id" => 1);
        ok!(Number(1f64), js["id"]);
        ok!(Number(&1f64), js.find("id").unwrap());
        assert_eq!(js.to_string(), r#"{"id": 1}"#.to_string());
        ok!(Number(&1f64), js.find_mut("id").unwrap());
        assert_eq!(js.to_string(), r#"{"id": 1}"#.to_string());
        js["id"] = 3.to_json();
        assert_eq!(js.to_string(), r#"{"id": 3}"#.to_string());
        ok!(Number(&2f64), js.update("id", 2).unwrap());
        assert_eq!(js.to_string(), r#"{"id": 2}"#.to_string());
        ok!(Number(2f64), js.discard("id").unwrap());
        assert_eq!(js.to_string(), r#"{}"#.to_string());

        let mut js = array![0, 1, 2];
        ok!(Number(0f64), js[0]);
        ok!(Number(&0f64), js.find(0).unwrap());
        assert_eq!(js.to_string(), r#"[0, 1, 2]"#.to_string());
        ok!(Number(&0f64), js.find_mut(0).unwrap());
        assert_eq!(js.to_string(), r#"[0, 1, 2]"#.to_string());
        js[0] = 3.to_json();
        assert_eq!(js.to_string(), r#"[3, 1, 2]"#.to_string());
        ok!(Number(&2f64), js.update(0, 2).unwrap());
        assert_eq!(js.to_string(), r#"[2, 1, 2]"#.to_string());
        ok!(Number(2f64), js.discard(0).unwrap());
        assert_eq!(js.to_string(), r#"[1, 2]"#.to_string());
    }

    #[test]
    fn test_convert() {
        let vec = vec![1, 2, 3];

        // encode Vec
        let js = crate::encode(&vec);
        assert_eq!("[1, 2, 3]", &js);

        // decode to Vec
        let vec_from_json: Vec<i32> = crate::decode(&js).unwrap();
        assert_eq!(vec, vec_from_json);

        use std::collections::HashMap;
        let mut map = HashMap::new();
        map.insert("name".to_string(), "Jack".to_string());
        map.insert("age".to_string(), "17".to_string());

        // encode HashMap
        let js = crate::encode(&map);
        assert_eq!(r#"{"age": "17", "name": "Jack"}"#, &js);

        // decode to HashMap
        let map_from_json: HashMap<String, String> = crate::decode(&js).unwrap();
        assert_eq!(map, map_from_json);

        let js = r#"{"numbers": [1, 2, 3]}"#;

        // get
        let n: i32 = crate::get(&js, "numbers.#0").unwrap();
        assert_eq!(n, 1);

        // set
        let js_after_set = crate::set(&js, "numbers.#4", 5).unwrap();
        assert_eq!(&js_after_set, r#"{"numbers": [1, 2, 3, null, 5]}"#);

        // del
        let js_after_del = crate::del(&js_after_set, "numbers.#3").unwrap();
        assert_eq!(&js_after_del, r#"{"numbers": [1, 2, 3, 5]}"#);
    }

    #[test]
    fn test_object_op() {
        let mut js = object! {
            "name" => "Jack",
            "age" => 24,
            "opt" => Json::Null,
        };
        js.insert("age", 26).unwrap();
        js.append("age", 28).unwrap();
        js.delete("opt").unwrap();
        assert_eq!(r#"{"age": 26, "age": 28, "name": "Jack"}"#, js.to_string());

        let vec = js.index_and_merge("age").unwrap();
        ok!(Number(&26f64), vec[0]);
        ok!(Number(&28f64), vec[1]);
    }

    #[test]
    fn test_array_op() {
        let mut js = array![1, 1, 3];
        js.push(4).unwrap();
        js.replace(1, 2).unwrap();
        js.remove(0).unwrap();
        assert_eq!(r#"[2, 3, 4]"#, js.to_string());
    }
}