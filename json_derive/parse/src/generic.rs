// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use std::ops::Deref;
use std::iter::FromIterator;
use crate::utils;
use crate::{Error, Result, Parse};
use crate::name::{Name, Type};

// Generic represents generic items in a group of angle brackets:
//
//     <T(: traits... + lifetimes... + (?Sized)) (= default), ...>
//
#[derive(Debug)]
pub struct Generics {
    pub items: Vec<GenericItem>,

    // n represents tokens num inside angle bracket
    n: usize,
}

// token_num means how many token it takes. In Generic case,
// count tokens is a complicate work, so we make a private
// field to record tokens num.
impl Generics {
    pub fn __token_num(&self) -> usize {
        // include '<' and '>'
        self.n + 2
    }

    pub fn only_idents(&self) -> String {
        if !self.items.is_empty() {
            format!("<{}>", self.items
                .iter()
                .map(|x| x.ident.clone())
                .collect::<Vec<String>>()
                .join(", "))
        } else {
            String::default()
        }
    }
}

impl ToString for Generics {
    fn to_string(&self) -> String {
        if !self.items.is_empty() {
            format!("<{}>", self.items
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", "))
        } else {
            String::default()
        }
    }
}

impl Parse for Generics {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing generics."));
        }

        // build iterator from input
        let mut iter = input
            .clone()
            .into_iter()
            // here we skip tokens before a "<" punct
            .skip_while(|x| {
                match x {
                    TokenTree::Punct(punct) => punct.as_char() != '<',
                    _ => true
                }
            });

        // stream is a vector containing the whole generic tokens
        let mut stream: Vec<TokenTree> = Vec::new();

        // n represents tokens num
        let mut n = 0;

        // because generic like group "<...>" is not a real group in rust
        // token, so we should handle it manually.
        // HINT: Fn trait should be handle specially because it contains a single
        // '>' to represent return type, we make a prev variable to check if it
        // is a "->" symbol.
        let mut prev = iter.next();
        let mut i = match prev {
            Some(TokenTree::Punct(ref punct)) if punct.as_char() == '<' => 1,
            _ => 0
        };

        // start handling from the second token, when hit on '<' then increase 1 for i,
        // when hit on '>' then decrease 1 for i. i == 0 meas all '<'&'>' pairs has been
        // processed. All tokens would be pushed into stream.
        loop {
            match iter.next() {
                Some(next) => {
                    // case #1: '<' -> n++
                    // case #2: '>' -> n--
                    match (&prev, &next) {
                        // HINT: '>' after '-' is not case #2
                        (Some(TokenTree::Punct(prev_punct)),
                            TokenTree::Punct(next_punct)) if prev_punct.as_char() == '-' &&
                            next_punct.as_char() == '>' => (),

                        (_, TokenTree::Punct(punct)) if punct.as_char() == '<' => i += 1,
                        (_, TokenTree::Punct(punct)) if punct.as_char() == '>' => i -= 1,
                        _ => ()
                    }

                    // break on i == 0, all pairs ready
                    if i == 0 {
                        break;
                    }

                    // set prev
                    prev = Some(next.clone());

                    // push TokenTree to stream for further program
                    stream.push(next);

                    // increase n for valid tokens
                    n += 1;
                }

                // hit end of TokenStream, break
                None => break,
            }
        }

        // n must be zero for a valid generic StreamToken
        if i > 0 {
            return Err(Error::new(format!("malformed generic: {}.", input)));
        }

        // inner items
        let items: Vec<GenericItem> = utils::split_tokens_by(TokenStream::from_iter(stream), ",")
            .into_iter()
            // remove empty token
            .filter(|x| !x.is_empty())
            // finally convert into GenericItem
            .map(|x| GenericItem::parse(x))
            // collect to a Result<Vec<GenericItem>>
            .collect::<Result<Vec<GenericItem>>>()?;

        Ok(Generics { items, n })
    }
}

// GenericItem represents an item of generic parameters:,
//
//     T(: traits... + lifetimes... + (?Sized)) (= default)
//
// Including name/ident, bounds(trait, lifetime and sized option)
// and default type. Traits and lifetimes store in two vector but
// sized option is a boolean value. Default value is usually unset
// so we make a Option<String> for it.
#[derive(Debug)]
pub struct GenericItem {
    pub ident: String,
    pub traits: Vec<Name>,
    pub lifetimes: Vec<Name>,
    pub sized: bool,
    pub default: Option<Type>,
}

impl GenericItem {
    // provide a shortcut to judge whether this generic parameter
    // is a lifetime, which should starts with a '\'' punct.
    pub fn is_lifetime(&self) -> bool {
        self.ident.starts_with('\'')
    }

    pub fn is_static(&self) -> bool {
        &self.ident == "'static"
    }
}

impl ToString for GenericItem {
    fn to_string(&self) -> String {
        let mut gen = self.ident.clone();

        let mut buf = self.traits
            .iter()
            .map(|x| x.to_string())
            .chain(self.lifetimes
                .iter()
                .map(|x| x.to_string()))
            .collect::<Vec<String>>();

        if !self.sized {
            buf.push("?Sized".to_string());
        }

        if buf.len() > 0 {
            gen.push_str(": ");
            gen.push_str(&buf.join(" + "));
        }

        if let Some(ref default) = self.default {
            gen.push_str(" = ");
            gen.push_str(&default.to_string());
        }

        gen
    }
}

impl Parse for GenericItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing generic items."));
        }

        // first handle default value, split by '='
        let tokens = utils::split_tokens_by(input.clone(), "=");

        // malformed generic
        if tokens.len() != 1 && tokens.len() != 2 {
            return Err(Error::new(format!("malformed generic: {}.", input)));
        }

        // preload generic item
        let mut item = GenericItem {
            ident: String::default(),
            traits: Vec::default(),
            lifetimes: Vec::default(),
            sized: true,
            default: None,
        };

        // before we start, we handle default value
        if tokens.len() > 1 {
            let token = tokens[1].clone();

            // giving it a check for default type because it is possible
            // to contain generic parameters in it.
            if let Ok(default) = Type::parse(token.clone()) {
                item.default = Some(default);
            } else {
                return Err(Error::new(format!("malformed default type for generic: {}.", token)));
            }
        }

        // start the generic parse
        let mut generic: Vec<TokenStream> = Vec::with_capacity(2);
        let mut token = TokenStream::new();
        let mut hit_colon = false;
        for e in tokens[0].clone() {
            if hit_colon {
                token.extend(Some(e));
            } else {
                match e {
                    TokenTree::Punct(ref punct) if punct.as_char() == ':' => {
                        hit_colon = true;
                        generic.push(token.clone());
                        token = TokenStream::new();
                    }
                    other => {
                        token.extend(Some(other))
                    }
                }
            }
        }
        generic.push(token);

        // malformed generic
        if generic.len() != 1 && generic.len() != 2 {
            return Err(Error::new(format!("malformed generic: {}.", input)));
        }

        // first we match generic name(Ident), get ident from group
        let ident_tokens = generic[0]
            .clone()
            .into_iter()
            .collect::<Vec<_>>();

        // match tokens in ident
        item.ident = match ident_tokens.len() {
            // T
            1 => {
                if let TokenTree::Ident(name) = &ident_tokens[0] {
                    name.to_string()
                } else {
                    return Err(Error::new(format!("generic name should be an Ident, got {}.", &ident_tokens[0])));
                }
            }

            // lifetime 'a
            // or const N
            2 => {
                match &ident_tokens[0] {
                    TokenTree::Punct(punct) if punct.as_char() == '\'' => {
                        if let TokenTree::Ident(name) = &ident_tokens[1] {
                            format!("'{}", name.to_string())
                        } else {
                            return Err(Error::new(format!("lifetime name should be an Ident, got {}.", &ident_tokens[1])));
                        }
                    }
                    TokenTree::Ident(ident) if &ident.to_string() == "const" => {
                        if let TokenTree::Ident(name) = &ident_tokens[1] {
                            format!("'{}", name.to_string())
                        } else {
                            return Err(Error::new(format!("const generic name should be an Ident, got {}.", &ident_tokens[1])));
                        }
                    }
                    _ => {
                        return Err(Error::new(format!("unexpected generic: {}.", ident_tokens.into_iter().collect::<TokenStream>())));
                    }
                }
            }

            // malformed
            _ => {
                return Err(Error::new(format!(
                    "malformed generic ident, expected 'a or T, got: {}.",
                    ident_tokens
                        .into_iter()
                        .collect::<TokenStream>()
                )));
            }
        };

        // then we match bounds, including traits, lifetime and sized
        if generic.len() > 1 {
            let bounds_tokens = utils::split_tokens_by(generic[1].clone(), "+")
                .into_iter()
                .map(|x| {
                    x.into_iter()
                        .collect::<Vec<TokenTree>>()
                })
                .collect::<Vec<_>>();

            for bound_tokens in bounds_tokens {
                match (bound_tokens.first(), bound_tokens.last()) {
                    (Some(TokenTree::Punct(punct)), Some(TokenTree::Ident(ident)))
                    if punct.as_char() == '?' && &ident.to_string() == "Sized" => {
                        item.sized = false;
                        continue;
                    }
                    (Some(TokenTree::Ident(ident1)), Some(TokenTree::Ident(ident2)))
                    if &ident1.to_string() == "Sized" && &ident2.to_string() == "Sized" => {
                        item.sized = true;
                        continue;
                    }
                    _ => ()
                }
                let bound = Name::parse(bound_tokens.into_iter().collect())?;
                if bound.is_lifetime() {
                    item.lifetimes.push(bound);
                } else {
                    item.traits.push(bound);
                }
            }
        }

        // return
        Ok(item)
    }
}

// Where represents where clauses, it looks like:
//
//     where T(: traits... + lifetimes... + (?Sized))
//
// Due to the similarity between Where and Generics,
// so we provide the same API as Generics by implements
// the Deref trait for Where.
#[derive(Debug)]
pub struct WhereClause(Generics);

impl WhereClause {
    pub fn __token_num(&self) -> usize {
        // include "where"
        self.n + 1
    }
}

impl Deref for WhereClause {
    type Target = Generics;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ToString for WhereClause {
    fn to_string(&self) -> String {
        if !self.items.is_empty() {
            format!("where {}", self.items
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", "))
        } else {
            String::default()
        }
    }
}

impl Parse for WhereClause {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing where clauses."));
        }

        // n represents tokens num
        let mut n = 0;

        // build stream from input
        let stream = input
            .clone()
            .into_iter()
            // here we skip tokens before "where"
            .skip_while(|x| {
                match x {
                    TokenTree::Ident(ident) => &ident.to_string() != "where",
                    _ => true
                }
            })
            // skip the "where" token
            .skip(1)
            // then we retain tokens before "{}" or ";"
            .take_while(|x| {
                match x {
                    TokenTree::Punct(punct) => punct.as_char() != ';',
                    TokenTree::Group(group) => group.delimiter() != Delimiter::Brace,
                    _ => true
                }
            })
            // count num of tokens
            .map(|x| {
                n += 1;
                x
            })
            .collect();

        // inner items
        let items: Vec<GenericItem> = utils::split_tokens_by(stream, ",")
            .into_iter()
            // remove empty token
            .filter(|x| !x.is_empty())
            // finally convert into GenericItem
            .map(|x| GenericItem::parse(x))
            // collect to a Result<Vec<GenericItem>>
            .collect::<Result<Vec<GenericItem>>>()?;

        Ok(WhereClause(Generics { items, n }))
    }
}
