// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use std::ops::{Deref, DerefMut};
use std::iter::FromIterator;
use std::collections::HashMap;
use crate::utils;
use crate::{Error, Result, Parse};

// A meta represents all #[...] form meta, it will
// parse all meta starts with '#'('#!' would not).
// All meta would be collected into the inner items(Vec<MetaItem>).
// Each #[...] is a MetaItem, and specially a name(...)
// group would contain a inner Meta wrapped by NestedMeta.
#[derive(Debug)]
pub struct Meta {
    pub items: Vec<MetaItem>
}

// token_num means how many token it takes. In Meta case,
// a '#' is a token, and a "[...]" is a token, so the token
// num is 2 times of the count of MetaItems.
impl Meta {
    pub fn __token_num(&self) -> usize {
        self.items.len() * 2
    }
}

impl ToString for Meta {
    fn to_string(&self) -> String {
        self.items
            .iter()
            .map(|x| x.to_string())
            .map(|x| format!("#[{}]", x))
            .collect::<Vec<String>>()
            .join("\n")
    }
}

// some useful methods to access inner MetaItem
impl Meta {
    // get item from Meta.items, different MetaIndex would return
    // different MetaItem:
    //     1. usize indexes Indent and Literal;
    //     2. &str indexes Pair;
    //     3. (&str, ()) index Group;
    //     4. (&str, MetaIndex) nested index Group and inner MetaItem;
    pub fn get<I: MetaIndex>(&self, index: I) -> Option<&I::Output> {
        index.index(self)
    }

    // get all Idents from Meta.items, return a vector of its &str
    pub fn idents(&self) -> Vec<&str> {
        self.items
            .iter()
            .filter_map(|x| {
                match x {
                    MetaItem::Ident(s) => Some(s.as_str()),
                    _ => None
                }
            })
            .collect()
    }

    // get all Literals from Meta.items, return a vector of &Lit
    pub fn literals(&self) -> Vec<&Lit> {
        self.items
            .iter()
            .filter_map(|x| {
                match x {
                    MetaItem::Literal(l) => Some(l),
                    _ => None
                }
            })
            .collect()
    }

    // get all Pairs from Meta.items, construct a HashMap<&str, &Lit>
    pub fn pairs(&self) -> HashMap<&str, &Lit> {
        self.items
            .iter()
            .filter_map(|x| {
                match x {
                    MetaItem::Pair { key, lit, .. } => Some((key.as_str(), lit)),
                    _ => None
                }
            })
            .collect()
    }

    // get all Groups from Meta.items, construct a HashMap<&str, &Meta>
    pub fn groups(&self) -> HashMap<&str, &NestedMeta> {
        self.items
            .iter()
            .filter_map(|x| {
                match x {
                    MetaItem::Group { ident, inner, .. } => Some((ident.as_str(), inner)),
                    _ => None
                }
            })
            .collect()
    }
}

impl Parse for Meta {
    fn parse(input: TokenStream) -> Result<Meta> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing meta."));
        }

        // build iterator from input, only include "#" and "[...]"
        let mut iter = input
            .clone()
            .into_iter()
            // skip until hit "#" and "[...]" tokens
            .skip_while(|x| {
                match x {
                    TokenTree::Punct(punct) => punct.as_char() != '#',
                    TokenTree::Group(group) => group.delimiter() != Delimiter::Bracket,
                    _ => true
                }
            })
            // here we retain "#" and "[...]" tokens
            .take_while(|x| {
                match x {
                    TokenTree::Punct(punct) => punct.as_char() == '#',
                    TokenTree::Group(group) => group.delimiter() == Delimiter::Bracket,
                    _ => false
                }
            });

        // items vector with predictable capacity
        let mut items = Vec::with_capacity(iter
            .clone()
            .filter(utils::punct_eq('#'))
            .count());

        // loop to parse MetaItems
        loop {
            match iter.next() {
                // hit '#' punct, represents a meta group
                Some(TokenTree::Punct(ref punct)) if punct.as_char() == '#' => {
                    // parse and push meta item to Meta vector
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        items.push(MetaItem::parse(group.stream())?);
                        continue;
                    }

                    // malformed meta
                    return Err(Error::new(format!("malformed meta: {}.", input)));
                }

                // case #1: hit other tokens, means meta ends, break loop;
                // case #2: hit end of stream, break loop;
                _ => break,
            }
        }

        // ...
        debug_assert_eq!(items.len(), items.capacity());

        // build meta
        Ok(Meta { items })
    }
}

// Lit represents all literal in Rust:
//     1. bytes collected into a Vec<u8>;
//     2. string collected as String;
//     3. integer collected as i32;
//     4. float collected as f64;
//     5. bool collected as bool;
// Boolean value is a special case, because Rust recognize true/false
// as ident so we have some special measurement.
#[derive(Debug)]
pub enum Lit {
    // b'...'
    // b"..."
    // br"..."
    // br#"..."#
    // br##"..."##
    Bytes { bytes: Vec<u8>, raw: String },

    // "..."
    // r"..."
    // r#"..."#
    // r##"..."##
    String { string: String, raw: String },

    // 123
    // 123i32
    // -123
    // -123i32
    // HINT: negative value is a special case.
    // HINT: only support i32 integer.
    Integer { integer: i32, raw: String },

    // 3.14
    // 3.14f64
    // -3.14
    // -3.14f64
    // HINT: negative value is a special case.
    Float { float: f64, raw: String },

    // true
    // false
    // HINT: boolean value is a ident.
    Boolean { boolean: bool, raw: String },
}

impl ToString for Lit {
    fn to_string(&self) -> String {
        match self {
            Lit::Bytes { raw, .. } => raw.clone(),
            Lit::String { raw, .. } => raw.clone(),
            Lit::Integer { raw, .. } => raw.clone(),
            Lit::Float { raw, .. } => raw.clone(),
            Lit::Boolean { raw, .. } => raw.clone(),
        }
    }
}

impl Lit {
    // get bytes from Lit
    pub fn bytes(&self) -> Option<&[u8]> {
        match self {
            Lit::Bytes { bytes, .. } => Some(bytes),
            _ => None
        }
    }

    // get string from Lit
    pub fn string(&self) -> Option<&str> {
        match self {
            Lit::String { string, .. } => Some(string),
            _ => None
        }
    }

    // get integer from Lit
    pub fn integer(&self) -> Option<i32> {
        match self {
            Lit::Integer { integer, .. } => Some(*integer),
            _ => None
        }
    }

    // get float from Lit
    pub fn float(&self) -> Option<f64> {
        match self {
            Lit::Float { float, .. } => Some(*float),
            _ => None
        }
    }

    // get boolean from Lit
    pub fn boolean(&self) -> Option<bool> {
        match self {
            Lit::Boolean { boolean, .. } => Some(*boolean),
            _ => None
        }
    }
}

macro_rules! fix_suffix {
    ( $ident:ident : $ty:ty  => ( $($lit:literal),+ ) ) => {{
        if false {
            "".parse::<$ty>()
        } $(else if $ident.ends_with($lit) {
            $ident.trim_end_matches($lit).parse::<$ty>()
        })+ else {
            $ident.parse::<$ty>()
        }
    }};
}

macro_rules! fix_bytes {
    ( $ident:ident => ( $(( $prefix:literal, $suffix:literal )),+ ) ) => {
        $(if $ident.starts_with($prefix) && $ident.ends_with($suffix) {
            return Ok(
                Lit::Bytes {
                    bytes: $ident
                        .trim_start_matches($prefix)
                        .trim_end_matches($suffix)
                        .as_bytes()
                        .to_vec(),
                    raw: $ident,
                }
            );
        })+
    };
}

macro_rules! fix_string {
    ( $ident:ident => ( $(( $prefix:literal, $suffix:literal )),+ ) ) => {
        $(if $ident.starts_with($prefix) && $ident.ends_with($suffix) {
            return Ok(
                Lit::String {
                    string: $ident
                        .trim_start_matches($prefix)
                        .trim_end_matches($suffix)
                        .to_string(),
                    raw: $ident,
                }
            );
        })+
    };
}

impl Parse for Lit {
    fn parse(input: TokenStream) -> Result<Lit> {
        // handle emtpy token
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing literal."));
        }

        // into iter
        let mut iter = input.clone().into_iter();

        // unwrap the only literal
        match iter.next().unwrap() {
            // parse bool
            TokenTree::Ident(ref ident) => {
                let raw = ident.to_string();
                if let Ok(boolean) = raw.parse::<bool>() {
                    return Ok(Lit::Boolean { boolean, raw });
                }
            }

            // parse literal
            TokenTree::Literal(ref lit) => {
                // convert to string first
                let raw = lit.to_string();

                // parse bytes
                fix_bytes!(raw => (("b\'", "\'"),
                                   ("b\"", "\""),
                                   ("br\"", "\""),
                                   ("br#\"", "\"#"),
                                   ("br##\"", "\"##")));

                // parse string
                fix_string!(raw => (("\"", "\""),
                                    ("r\"", "\""),
                                    ("r#\"", "\"#"),
                                    ("r##\"", "\"##")));

                // parse int
                if let Ok(integer) = fix_suffix!(raw: i32 => (
                                            "u8", "u16", "u32", "u64",
                                            "u128", "usize", "i8", "i16",
                                            "i32", "i64", "i128", "isize")) {
                    return Ok(Lit::Integer { integer, raw });
                }

                // parse float
                if let Ok(float) = fix_suffix!(raw: f64 => ("f32", "f64")) {
                    return Ok(Lit::Float { float, raw });
                }
            }

            // other case
            other => {
                return Err(Error::new(format!("expect literal or true/false ident, got {}.", other)));
            }
        }

        if let Some(_) = iter.next() {
            return Err(Error::new("literal should only contain one token."));
        }

        Err(Error::new(format!("error parsing literal value {}.", input)))
    }
}

// NestedMeta is a wrap of Meta, represents inner meta in a group.
// Due to it does not have a "#[...]" form, so we should implement
// ToString for NestedMeta separately. And further more, we provide
// the same API as Meta by implements the Deref trait for NestedMeta.
#[derive(Debug)]
pub struct NestedMeta(Meta);

impl Deref for NestedMeta {
    type Target = Meta;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NestedMeta {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ToString for NestedMeta {
    fn to_string(&self) -> String {
        self.items
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    }
}

impl Parse for NestedMeta {
    fn parse(input: TokenStream) -> Result<NestedMeta> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing nested meta item."));
        }

        // convert single TokenStream to Vec<MetaItem> split by ','
        let items: Vec<MetaItem> = input
            // into iterator
            .into_iter()
            // collect to vec
            .collect::<Vec<TokenTree>>()
            // split by punct ','
            .split(utils::punct_eq(','))
            // remove empty token
            .filter(|&x| !x.is_empty())
            // convert to vector
            .map(|x| x.to_vec())
            // then convert into TokenStream
            .map(|x| TokenStream::from_iter(x))
            // finally convert into MetaItem
            .map(|x| MetaItem::parse(x))
            // collect to a Result<Vec<MetaItem>>
            .collect::<Result<Vec<MetaItem>>>()?;

        // build Meta
        Ok(NestedMeta(Meta { items }))
    }
}


// MetaItem represents each items in Meta.
// A #[...] is a MetaItem, and items in name(...)
// group are MetaItems.
#[derive(Debug)]
pub enum MetaItem {
    // #[ident]
    Ident(String),

    // #["literal"]
    Literal(Lit),

    // #[key = "literal"]
    // #[key: "literal"]
    Pair { key: String, lit: Lit, eq: char },

    // #[name(...)]
    Group { ident: String, inner: NestedMeta },
}

impl ToString for MetaItem {
    fn to_string(&self) -> String {
        match self {
            MetaItem::Ident(ident) => ident.clone(),
            MetaItem::Literal(lit) => lit.to_string(),
            MetaItem::Pair { key, lit, eq } => format!("{} {} {}", key, eq, lit.to_string()),
            MetaItem::Group { ident, inner } => format!("{}({})", ident, inner.to_string()),
        }
    }
}

impl MetaItem {
    // get &str from ident
    pub fn ident(&self) -> Option<&str> {
        match self {
            MetaItem::Ident(s) => Some(s),
            _ => None
        }
    }

    // get &Lit from literal
    pub fn literal(&self) -> Option<&Lit> {
        match self {
            MetaItem::Literal(l) => Some(l),
            _ => None
        }
    }

    // get (key: &str, lit: &Lit) from pair
    pub fn pair(&self) -> Option<(&str, &Lit)> {
        match self {
            MetaItem::Pair { key, lit, .. } => Some((key, lit)),
            _ => None
        }
    }

    // get (ident: &str, inner: &Meta) from group
    pub fn group(&self) -> Option<(&str, &Meta)> {
        match self {
            MetaItem::Group { ident, inner, .. } => Some((ident, inner)),
            _ => None
        }
    }
}

impl Parse for MetaItem {
    fn parse(input: TokenStream) -> Result<MetaItem> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing meta items."));
        }

        // TokenStream[TokenTree, TokenTree, TokenTree]
        let meta = input
            .clone().
            into_iter().
            collect::<Vec<TokenTree>>();

        // for different case
        match meta.len() {
            // ident or literal
            1 => {
                match &meta[0] {
                    // parse indent
                    TokenTree::Ident(_) => {
                        // special boolean case
                        if let Ok(b @ Lit::Boolean { .. }) = Lit::parse(input.clone()) {
                            return Ok(MetaItem::Literal(b));
                        }

                        // normal ident
                        return Ok(MetaItem::Ident(input.to_string()));
                    }

                    // parse literal
                    TokenTree::Literal(_) => {
                        return Ok(MetaItem::Literal(Lit::parse(input)?));
                    }

                    // malformed meta
                    _ => return Err(Error::new(format!("expect ident or literal, got: {}.", input))),
                }
            }

            // group => name(...)
            // OR negative value: -123/-3.14
            2 => {
                // parse negative value
                if utils::punct_eq('-')(&meta[0]) {
                    match Lit::parse(meta[1].clone().into())? {
                        Lit::Integer { integer, raw } => return Ok(MetaItem::Literal(Lit::Integer { integer: -integer, raw })),
                        Lit::Float { float, raw } => return Ok(MetaItem::Literal(Lit::Float { float: -float, raw })),
                        _ => return Err(Error::new(format!("error parsing negative literal: {}.", input)))
                    }
                }

                // parse name(...)
                if let TokenTree::Ident(ident) = &meta[0] {
                    if let TokenTree::Group(group) = &meta[1] {
                        // ident & inner
                        let ident = ident.to_string();
                        let inner = NestedMeta::parse(group.stream())?;

                        // valid return
                        return Ok(MetaItem::Group { ident, inner });
                    }
                }

                // malformed meta
                return Err(Error::new(format!("expect name(...), got: {}.", input)));
            }

            // kv pairs: key = lit
            3 => {
                // key and lit for KeyValue meta
                let key;
                let lit;
                let eq;

                // match eq token '='/':'
                match &meta[1] {
                    TokenTree::Punct(punct) if punct.as_char() == '=' => { eq = '='; }
                    TokenTree::Punct(punct) if punct.as_char() == ':' => { eq = ':'; }
                    _ => return Err(Error::new(format!("expect key = value, got: {}.", input)))
                }

                // match key ident
                match &meta[0] {
                    TokenTree::Ident(ident) => { key = ident.to_string(); }
                    other => return Err(Error::new(format!("expect ident key, got: {}.", other)))
                }

                // match value literal
                lit = Lit::parse(meta[2].clone().into())?;

                // data has been validate
                return Ok(MetaItem::Pair { key, lit, eq });
            }

            // kv pairs with negative value: key = -123
            4 => {
                // key and lit for KeyValue meta
                let key;
                let lit;
                let eq;

                // match eq token '='/':'
                match &meta[1] {
                    TokenTree::Punct(punct) if punct.as_char() == '=' => { eq = '='; }
                    TokenTree::Punct(punct) if punct.as_char() == ':' => { eq = ':'; }
                    _ => return Err(Error::new(format!("expect key = value, got: {}.", input)))
                }

                // match key ident
                match &meta[0] {
                    TokenTree::Ident(ident) => { key = ident.to_string(); }
                    other => return Err(Error::new(format!("expect ident key, got: {}.", other)))
                }

                // match negative value literal
                if utils::punct_eq('-')(&meta[2]) {
                    match Lit::parse(meta[3].clone().into())? {
                        Lit::Integer { integer, raw } => { lit = Lit::Integer { integer: -integer, raw }; }
                        Lit::Float { float, raw } => { lit = Lit::Float { float: -float, raw }; }
                        _ => return Err(Error::new(format!("error parsing negative literal: - {}.", &meta[3])))
                    }
                } else {
                    return Err(Error::new(format!("negative value should starts with a '-' punct, got {}.", &meta[2])));
                }

                // data has been validate
                return Ok(MetaItem::Pair { key, lit, eq });
            }

            // malformed meta
            _ => {
                return Err(Error::new(format!("malformed meta item: {}.", input)));
            }
        }
    }
}

// meta index, use for indexing MetaItems in Meta.
// See comments in Meta.get() method.
pub trait MetaIndex {
    type Output;
    fn index(self, meta: &Meta) -> Option<&Self::Output>;
}

// integer index, only Ident/Literal would be indexed
impl MetaIndex for usize {
    type Output = MetaItem;
    fn index(self, meta: &Meta) -> Option<&Self::Output> {
        meta.items
            .iter()
            .filter(|&x| {
                match x {
                    MetaItem::Ident(_) => true,
                    MetaItem::Literal(_) => true,
                    _ => false
                }
            })
            .nth(self)
    }
}

// &integer index, only Ident/Literal would be indexed
impl MetaIndex for &usize {
    type Output = MetaItem;
    fn index(self, meta: &Meta) -> Option<&Self::Output> {
        meta.items
            .iter()
            .filter(|&x| {
                match x {
                    MetaItem::Ident(_) => true,
                    MetaItem::Literal(_) => true,
                    _ => false
                }
            })
            .nth(*self)
    }
}

// &str index, only Pair would be indexed
impl MetaIndex for &str {
    type Output = Lit;
    fn index(self, meta: &Meta) -> Option<&Self::Output> {
        let pair = meta.items
            .iter()
            .find(|&x| {
                if let MetaItem::Pair { key, .. } = x {
                    return key == self;
                }
                false
            });

        match pair {
            Some(MetaItem::Pair { lit, .. }) => {
                return Some(lit);
            }
            _ => return None,
        }
    }
}

// (&str, T: MetaIndex) index, only Group would be indexed
// this is a nested index, it will first index Meta by &str,
// and then index inner Meta by T: MetaIndex.
impl<T: MetaIndex> MetaIndex for (&str, T) {
    type Output = T::Output;
    fn index(self, meta: &Meta) -> Option<&Self::Output> {
        // first find group by ident
        let group = meta.items
            .iter()
            .find(|&x| {
                if let MetaItem::Group { ident, .. } = x {
                    return ident == self.0;
                }
                false
            });

        match group {
            // index inner Meta by T
            Some(MetaItem::Group { inner, .. }) => {
                return self.1.index(inner);
            }

            // other case return None
            _ => return None,
        }
    }
}

// (&str, ()) index, only Group would be indexed,
// differ to (&str, T: MetaIndex),this would return
// the group's inner Meta.
impl MetaIndex for (&str, ()) {
    type Output = NestedMeta;
    fn index(self, meta: &Meta) -> Option<&Self::Output> {
        match meta.items
            .iter()
            .find(|&x| {
                if let MetaItem::Group { ident, .. } = x {
                    return ident == self.0;
                }
                false
            }) {
            Some(MetaItem::Group { inner, .. }) => {
                Some(inner)
            }
            _ => None
        }
    }
}
